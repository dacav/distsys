-module(faildet).
-author("Giovanni Simoni").
-export([new/1, merge/2, periodic_purge/1, get_neighbors/1]).

-record(fd, {
    known=gb_trees:empty(),
    tfail
}).

-record(neighbor, {
    heartbeat=0,
    ttl
}).

new (TFail) ->
    #fd{
        tfail = TFail
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

update (Pid, Heartbeat, Known, TFail) ->
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
                ttl = TFail
            }
        end,
    New = #neighbor {
    },
    update_pid(Pid, Known, Action, Update, New).

merge (KnownList, FD) ->
    Update =
        fun ({Pid, Hb}, {Chs, Kns}) ->
            {IsUpdated, NewKns} = update(Pid, Hb, Kns, FD#fd.tfail),
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

periodic_purge (FD) ->
    Known = FD#fd.known,
    Action =
        fun (Record) ->
            case Record#neighbor.ttl of
                0 -> drop;
                _ -> update
            end
        end,
    Update =
        fun (Record) ->
            Record#neighbor{
                ttl = Record#neighbor.ttl - 1
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

get_neighbors (FD) ->
    gb_trees:keys(FD#fd.known).
