-module(faildet).
-author("Giovanni Simoni").
-export([new/3, merge/2, period/1, get_neighbors/1,
         get_gossip_message/1, insert_neighbor/2]).

-record(fd, {
    known=gb_trees:empty(),

    % Timing
    tfail,
    tcleanup,
    tgossip,

    heartbeat = 0,  % My own heartbeat
    gossip_cd       % Countdown to gossip time
}).

-record(neighbor, {
    heartbeat=-1,
    ttl,            % Time to Live
    ttk             % Time to Keep
}).

new (TFail, TCleanup, TGossip)
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
                {insert, NewKns} -> {[Pid | Brn], NewKns};
                {_, NewKns} -> {Brn, NewKns}
            end
        end,
    {Born, NewKnown} = lists:foldl(Update, {[], FD#fd.known},
                                   KnownList),
    {Born, FD#fd{
               known = NewKnown
           }
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
    {Dead, FD#fd{
             known = NewKnown,
             gossip_cd = GossipCD,
             heartbeat = Heartbeat
            }
    }.

get_neighbors (#fd{ known=Known }) ->
    {gb_trees:size(Known), gb_trees:keys(Known)}.

get_gossip_message (#fd{ known=Known, gossip_cd=GCD, heartbeat=HB }) ->
    case GCD of
        expired ->
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
            [{self(), HB} | AllOther];
        _ ->
            none
    end.

insert_neighbor (Pid, FD = #fd{}) ->
    Record = #neighbor {
        heartbeat = -1,
        ttl = FD#fd.tfail,
        ttk = FD#fd.tcleanup
    },
    Known = FD#fd.known,
    FD#fd{
        known = gb_trees:enter(Pid, Record, Known)
    }.
