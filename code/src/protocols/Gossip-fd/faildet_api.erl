-module(faildet_api).
-author("Giovanni Simoni").
-export([send_known_list/2, bcast_known_list/1, greet/1]).

-import(gfd_api).

envelop (KnownList) ->
    {known_list, KnownList}.

send_known_list (Pid, KnownList) when is_pid(Pid) ->
    gfd_api:faildet_send(Pid, envelop(KnownList));

send_known_list (Peers, KnownList) ->
    case Peers of
        {0, _} ->
            bcast_known_list(KnownList);
        {NPids, Pids} ->
            % With a 10% probability send broadcast anyways
            case random:uniform() < 0.1 of
                false -> random_send_known_list(KnownList, Pids, NPids);
                true -> bcast_known_list(KnownList)
            end,
            ok
    end.

bcast_known_list (KnownList) ->
    gfd_api:faildet_bcast(envelop(KnownList)).

random_send_known_list (KnownList, Pids, NPids) ->
    gfd_api:faildet_send(lists:nth(random:uniform(NPids), Pids),
                         envelop(KnownList)).

greet (Peer) ->
    gfd_api:faildet_greet(Peer).
