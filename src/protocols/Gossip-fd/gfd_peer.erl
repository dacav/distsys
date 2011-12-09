-module(gfd_peer).
-author("Giovanni Simoni").
-behavior(gen_peer).
-export([init/1, handle_message/3, handle_introduction/3,
         handle_info/2, handle_beacon/1]).

-import(peer_chan).
-import(peer_ctrl).

-record(status, {
    fd,
    bcastprob
}).

init ({TFail, TCleanup, TGossip}) ->
    Status = #status{
        fd = faildet:new(TFail, TCleanup, TGossip)
    },
    peer_ctrl:notify_spawn(),
    {ok, Status}.

handle_message (_From, {gossip_faildet, KnownList}, Status) ->
    {Born, NewFD} = faildet:merge(KnownList, Status#status.fd),
    case Born of
        [] -> ok;
        _ -> log_serv:log("New neighbors: ~p", [Born])
    end,
    NewStatus = Status#status {
        fd = NewFD
    },
    {ok, NewStatus}.

handle_introduction (_From, Pid, Status = #status{ fd=FD }) ->
    NewStatus = Status#status{
        fd=faildet:insert_neighbor(Pid, FD)
    },
    {ok, NewStatus}.

handle_info (_Noise, _Prvt) ->
    {ok, nil}.

handle_beacon (Status = #status{ fd=FD }) ->
    {Dead, NewFD} = faildet:period(FD),
    case Dead of
        [] -> ok;
        _ -> log_serv:log("Dead neigbors: ~p", [Dead])
    end,
    case faildet:get_gossip_message(FD) of
        none ->
            ok;
        Msg ->
            Neighbors = faildet:get_neighbors(FD),
            log_serv:log("Propagation: ~p", [element(2, Neighbors)]),
            send_random_peer(Neighbors, Msg)
    end,
    NewStatus = Status#status{
        fd = NewFD
    },
    {ok, NewStatus}.

% -----------------------------------------------------------------------
% Utility functions
% -----------------------------------------------------------------------

send_random_peer (Peers, Msg) ->
    case Peers of
        {0, _} ->
            ok;
        {NPids, Pids} ->
            gossip_send(lists:nth(random:uniform(NPids), Pids), Msg)
    end.

gossip_send (To, Msg) ->
    peer_chan:send(To, {gossip_faildet, Msg}).
