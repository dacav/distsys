-module(faildet).
-author("Giovanni Simoni").

-behavior(gen_peer).
% API for Failure Detector update system, behaving like a peer
-export([init/1, handle_message/3, handle_introduction/3,
         handle_info/2, handle_beacon/1]).

% API for Failure Detector data reading.
-export([get_neighbors/1, get_last_born/1, get_last_dead/1]).

-import(faildet_api).

-record(fd, {
    known=gb_trees:empty(),

    % Timing
    tfail,
    tcleanup,
    tgossip,

    heartbeat = 0,      % My own heartbeat
    gossip_cd,          % Countdown to gossip time

    last_born = [],     % List of recently appeared peers
    last_dead = [],     % List of recently disappeared peers

    stats_enabled=false
}).

-record(neighbor, {
    heartbeat=-1,
    ttl,            % Time to Live
    ttk             % Time to Keep
}).

% ------------------------------------------------------------------------
% gen_peer logic
% ------------------------------------------------------------------------

init ({TFail, TCleanup, TGossip})
        when    is_number(TFail)
        andalso is_number(TCleanup)
        andalso is_number(TGossip)
        andalso TFail > 0
        andalso TCleanup > 0
        andalso TGossip > 0
     ->
    #fd{
        tfail = TFail,
        tcleanup = TCleanup,
        tgossip = TGossip,
        gossip_cd = TGossip
    }.

handle_message (_From, {known_list, KnownList}, FD = #fd{}) ->
    NewFD = merge(KnownList, FD),
    do_stats(NewFD),
    {ok, NewFD};

handle_message (keeper, {stats_enabled, Setting}, FD=#fd{}) ->
    {ok, FD#fd{ stats_enabled=Setting }}.

handle_introduction (_From, Pid, FD = #fd{}) ->
    NewFD = insert_neighbor(Pid, FD),
    do_stats(NewFD),
    {ok, NewFD}.

handle_info (_, S) ->
    {ok, S}.

handle_beacon (FD=#fd{}) ->
    NewFD = period(FD),
    case ready_message(FD) of
        none ->
            ok;
        KnownList ->
            faildet_api:send_known_list(get_neighbors(FD), KnownList)
    end,
    do_stats(NewFD),
    {ok, NewFD}.

% ------------------------------------------------------------------------
% Failure detector internal logic
% ------------------------------------------------------------------------

do_stats (#fd{ known=Known, stats_enabled=true }) ->
    log_serv:est_node_count(gb_trees:size(Known));
do_stats (_) ->
    ok.

update_pid (Pid, Known, ToDo, Update, New) ->
    case gb_trees:lookup(Pid, Known) of
        none ->
            {insert, gb_trees:insert(Pid, Update(New), Known)};
        {value, Current} ->
            case ToDo(Current) of
                leave ->
                    {leave, Known};
                update ->
                    {update, gb_trees:update(Pid, Update(Current), Known)};
                drop ->
                    {drop, gb_trees:delete(Pid, Known)}
            end
    end.

update (Pid, Heartbeat, Known, TFail, TCleanup) ->
    Action =
        fun (Record) ->
            case Record#neighbor.heartbeat < Heartbeat of
                true -> update;
                false -> leave
            end
        end,
    Update =
        fun (Record) ->
            Record#neighbor{
                heartbeat = Heartbeat,
                ttl = TFail,
                ttk = TCleanup
            }
        end,
    New = #neighbor{},
    update_pid(Pid, Known, Action, Update, New).

merge (KnownList, FD) ->
    Update =
        fun ({Pid, Hb}, {Brn, Kns}) ->
            case update(Pid, Hb, Kns, FD#fd.tfail,
                        FD#fd.tcleanup) of
                {insert, NewKns} ->
                    {[Pid | Brn], NewKns};
                {_, NewKns} -> {Brn, NewKns}
            end
        end,
    {Born, NewKnown} = lists:foldl(Update, {[], FD#fd.known},
                                   KnownList),
    FD#fd{
        known = NewKnown,
        last_born = Born
    }.

purge_iteration (Known, Dead, Action, Update, Iterator) ->
    case gb_trees:next(Iterator) of
        {Pid, _, Next} ->
            {What, NewKnown} = update_pid(Pid, Known, Action,
                                          Update, none),
            NewDead =
                case What of
                    drop -> [Pid | Dead];
                    _ -> Dead
                end,
            purge_iteration(NewKnown, NewDead, Action,
                            Update, Next);
        none ->
            {Dead, Known}
    end.

purge_known (Known) ->
    Action =
        fun (Record) ->
            case Record#neighbor.ttk of
                0 -> drop;
                _ -> update
            end
        end,
    Update =
        fun (Record) ->
            TTKOrig = Record#neighbor.ttk,
            {TTK, TTL} =
                case Record#neighbor.ttl of
                    0 -> {TTKOrig - 1, 0};
                    TTLOrig -> {TTKOrig, TTLOrig - 1}
                end,
            Record#neighbor{
                ttl = TTL,
                ttk = TTK
            }
        end,
    {Dead, NewKnown} = purge_iteration(Known, [], Action, Update,
                                       gb_trees:iterator(Known)),
    % If we deleted the 80% of the nodes, the tree gets re-balanced
    % (totally arbitrary)
    case gb_trees:size(NewKnown) < 0.8 * gb_trees:size(Known) of
        true -> {Dead, gb_trees:balance(NewKnown)};
        false -> {Dead, NewKnown}
    end.

period (FD = #fd{ known = Known, heartbeat = HB }) ->
    {GossipCD, Heartbeat} =
        case FD#fd.gossip_cd of
            expired -> {FD#fd.tgossip, HB};
            1 -> {expired, HB + 1};
            N -> {N - 1, HB}
        end,
    {Dead, NewKnown} = purge_known(Known),
    FD#fd{
        known = NewKnown,
        gossip_cd = GossipCD,
        heartbeat = Heartbeat,
        last_dead = Dead
    }.

get_neighbors (#fd{ known=Known }) ->
    {gb_trees:size(Known), gb_trees:keys(Known)}.

forge_message (#fd{ known=Known, heartbeat=HB }) ->
    IsAlive =
        fun ({_, Record}) ->
            Record#neighbor.ttl > 0
        end,
    Unpack =
        fun ({Pid, Record}) ->
            {Pid, Record#neighbor.heartbeat}
        end,
    KnownList = gb_trees:to_list(Known),
    AllOther = lists:map(Unpack,
                         lists:filter(IsAlive, KnownList)),
    [{self(), HB} | AllOther].

ready_message (FD = #fd{ gossip_cd=GCD }) ->
    case GCD of
        expired ->
            forge_message(FD);
        _ ->
            none
    end.

insert_neighbor (Pid, FD = #fd{ known=Known, tfail=TFail,
                                tcleanup=TCleanup}) ->
    FD#fd{
        known=element(2, update(Pid, -1, Known, TFail, TCleanup))
    }.

get_last_born (#fd{ last_born=LB }) -> LB.
get_last_dead (#fd{ last_dead=LD }) -> LD.
