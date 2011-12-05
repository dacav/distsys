-module(pingpong_keeper).
-author("Giovanni Simoni").
-behavior(gen_keeper).
-export([init/1, handle_spawn_notify/2, handle_result_notify/3,
         handle_term_notify/4, handle_info/2]).

-import(keeper_inject).
-import(keeper_proto).

-record(status, {waiting_for,
                 spawned,
                 npeers}).

init (NPeers) ->
    keeper_proto:enable_beacon(5000),
    PeersRef0 =
        case keeper_proto:add_peers(NPeers, pingpong_peer, [nil]) of
            {ok, Lst0} -> Lst0;
            _ -> throw(no_spawn)
        end,
    PeersRef1 =
        case keeper_proto:add_peers(1, pingpong_peer_native, [nil]) of
            {ok, Lst1} -> Lst1;
            _ -> throw(no_spawn)
        end,
    PeersRef = PeersRef0 ++ PeersRef1,
    Peers = lists:map(fun (T) -> element(1, T) end, PeersRef),
    log_serv:log("Spawning ~p peers...\n> ~p", [NPeers, Peers]),
    {ok, #status{waiting_for=NPeers, spawned=Peers, npeers=NPeers}}.

decrease_waiting (Status) ->
    #status{waiting_for = Status#status.waiting_for - 1,
            spawned = Status#status.spawned,
            npeers = Status#status.npeers}.

kill_and_remove (Pid, Status) ->
    log_serv:log("killing and removing ~p", [Pid]),
    exit(Pid, kill),
    Spawned = lists:filter(fun (X) -> X =/= Pid end,
                           Status#status.spawned),
    case Spawned of
        [] -> stop;
        _ -> #status{waiting_for = Status#status.waiting_for,
                     spawned = Spawned,
                     npeers = Status#status.npeers - 1}
    end.

meet ([], Y) ->
    erlang:error(assertion, [[], Y]);
meet ([X], First) ->
    keeper_inject:introduce(X, First);
meet ([X, Y | Others], First) ->
    keeper_inject:introduce(X, Y),
    meet([Y | Others], First).

meet (All = [Fst | _]) ->
    meet(All, Fst).

run_pingpong(Spawned, N) ->
    % Let them meet each other!
    meet(Spawned),
    % Random selection of a starting pid
    SelPid = lists:nth(random:uniform(N), Spawned),
    log_serv:log("Injecting on ~p", [SelPid]),
    % Go!
    Homework = 3 * N,
    keeper_inject:send(SelPid, Homework),
    ok.

handle_spawn_notify (Pid, #status{waiting_for=0}) ->
    log_serv:log("Peer ~p seems to be spawned twice!", [Pid]),
    {error, phantom_peer};

handle_spawn_notify (Pid, Status) ->
    case Status#status.waiting_for of
        1 ->
            log_serv:log("Confirm by ~p. Last ok, ready to go!", [Pid]),
            run_pingpong(Status#status.spawned,
                         Status#status.npeers);
        N ->
            log_serv:log("Confirm by ~p. Waiting for ~p nodes...",
                         [Pid, N])
    end,
    {ok, decrease_waiting(Status)}.

handle_result_notify (Pid, Result, Status) ->
    log_serv:log("Result from ~p: ~p", [Pid, Result]),
    case kill_and_remove(Pid, Status) of
        stop ->
            log_serv:log("No more processes. Game over."),
            keeper_proto:disable_beacon(),
            stop;
        NewStatus ->
            log_serv:log("Killed the process."),
            {ok, NewStatus}
    end.

handle_term_notify (Pid, _Ref, Reason, Status) ->
    log_serv:log("Terminated ~p: ~p", [Pid, Reason]),
    case Reason of
        crashed ->
            log_serv:log("We had a crash. Game over"),
            lists:foldl(fun kill_and_remove/2, Status,
                        Status#status.spawned),
            keeper_proto:disable_beacon(),
            stop;
        killed ->
            log_serv:log("Killed successfully. Going on..."),
            run_pingpong(Status#status.spawned,
                         Status#status.npeers),
            {ok, Status}
    end.

handle_info (Info, Status) ->
    log_serv:log("Got info: ~p", [Info]),
    {ok, Status}.
