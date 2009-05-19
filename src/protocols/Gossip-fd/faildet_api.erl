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
            ok;
        {NPids, Pids} ->
            gfd_api:faildet_send(lists:nth(random:uniform(NPids), Pids),
                                 envelop(KnownList)),
            ok
    end.

bcast_known_list (KnownList) ->
    gfd_api:faildet_bcast(envelop(KnownList)).

greet (Peer) ->
    gfd_api:faildet_greet(Peer).
