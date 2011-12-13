-module(faildet_api).
-author("Giovanni Simoni").
-export([send_known_list/2]).

-import(gfd_api).

send_known_list (Peers, KnownList) ->
    case Peers of
        {0, _} ->
            ok;
        {NPids, Pids} ->
            gfd_api:faildet_send(lists:nth(random:uniform(NPids), Pids),
                                 {known_list, KnownList}),
            ok
    end.
