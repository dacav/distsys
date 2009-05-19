-module(faildet).
-author("Giovanni Simoni").
-export([new/2, merge/2, periodic_purge/1, get_neighbors/1,
         get_gossip_message/1]).

-record(fd, {
    known=gb_trees:empty(),
    tfail,
    tcleanup
}).

-record(neighbor, {
    heartbeat=0,
    ttl,            % Time to Live
    ttk             % Time to Keep
}).

new (TFail, TCleanup) when is_number(TFail)
                      andalso is_number(TCleanup)
                      andalso TFail > 0
                      andalso TCleanup > 0 ->
    #fd{
        tfail = TFail,
        tcleanup = TCleanup
    }.

update_pid (Pid, Known, ToDo, Update, New) ->
    case gb_trees:lookup(Pid, Known) of
        none ->
            {true, gb_trees:insert(Pid, Update(New), Known)};
        {value, Current} ->
            case ToDo(Current) of
                leave ->
                    {false, Known};
                update ->
                    {true, gb_trees:update(Pid, Update(Current), Known)};
                drop ->
                    {true, gb_trees:delete(Pid, Known)}
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
    New = #neighbor {
    },
    update_pid(Pid, Known, Action, Update, New).

merge (KnownList, FD) ->
    Update =
        fun ({Pid, Hb}, {Chs, Kns}) ->
            {IsUpdated, NewKns} = update(Pid, Hb, Kns, FD#fd.tfail,
                                         FD#fd.tcleanup),
            case IsUpdated of
                true -> {[Pid | Chs], NewKns};
                false -> {Chs, NewKns}
            end
        end,
    {Changed, NewKnown} = lists:foldl(Update, {[], FD#fd.known},
                                      KnownList),
    {Changed, FD#fd{known=NewKnown}}.

purge_iteration (Known, Action, Update, Iterator) ->
    case gb_trees:next(Iterator) of
        {Pid, _, Next} ->
            {_, NewKnown} = update_pid(Pid, Known, Action, Update, none),
            purge_iteration(NewKnown, Action, Update, Next);
        none ->
            Known
    end.

periodic_purge (FD = #fd{ known=Known }) ->
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
    NewKnown = purge_iteration(Known, Action, Update,
                               gb_trees:iterator(Known)),
    FD#fd{
        known =
            case gb_trees:size(NewKnown) < 0.8 * gb_trees:size(Known) of
                true -> gb_trees:balance(NewKnown);
                false -> NewKnown
            end
    }.

get_neighbors (#fd{ known=Known }) ->
    gb_trees:keys(Known).

get_gossip_message (#fd{ known=Known}) ->
    IsAlive =
        fun ({_, Record}) ->
            Record#neighbor.ttl > 0
        end,
    Unpack =
        fun ({Pid, Record}) ->
            {Pid, Record#neighbor.heartbeat}
        end,
    KnownList = gb_trees:to_list(Known),
    lists:map(Unpack, lists:filter(IsAlive, KnownList)).

