-module(gfd_keeper).
-author("Giovanni Simoni").
-behavior(gen_keeper).
-export([init/1, handle_spawn_notify/2, handle_result_notify/3,
         handle_term_notify/4, handle_info/2]).

-import(keeper_inject).
-import(keeper_proto).

-import(result).

-record(status, {
    alive=gb_sets:new(),  % Set of spawned nodes
    result=result:new(),  % Results from node (stats)
    stop=false,           % Termination in progress

    % Initialization parameters, commented later
    npeers,
    tbeacon,
    beaconwait,

    statpeers_ratio,
    statpeers=[]
}).

% Functioning logic for keeper:
%
% The keeper starts a certain number of peers (provided by the NPeers
% parameter). It each time a node is spawned the keeper introduces two
% peers.
%
% When all nodes have been spawned, the keeper starts the beacon: this
% will trigger the gossip algorithm running on the peers, which will
% allow node to know each other.
%
% Params
% - PeerParam is a tuple {TFail, TCleanup, TGossip};
% - NPeers is simply the number of spawned processes;
% - TBeacon is the beaconing interval (milliseconds);
% - BeaconWait is the number of beacon intervals to be waited before
%   starting the first round

init ({FDParams = {_,_,_}, NPeers, StatPeers, TBeacon, BeaconWait}) ->
    random:seed(now()), % Necessary for random peer introduction
    keeper_proto:add_peers(NPeers, gfd_peer, FDParams),
    Status = #status{
        npeers = NPeers,
        tbeacon = TBeacon,
        beaconwait = BeaconWait,
        statpeers_ratio = StatPeers
    },
    {ok, Status}.

handle_spawn_notify (Pid, Status = #status{ alive=Alive, npeers=N }) ->
    % We introduce the new pid to a random process among the spawned ones.
    introduce_random(Pid, Alive),
    NewAlive = gb_sets:insert(Pid, Alive),
    NewStatus0 = Status#status{
        alive = NewAlive
    },
    NAlive = gb_sets:size(NewAlive),
    log_serv:node_count(NAlive),
    NewStatus1 =
        case NAlive of
            N ->
                % Upon all nodes have been correctly spawned, we start thexs
                % protocol
                log_serv:log("Nodes started: N=~p", [N]),
                prepare_protocol(NewStatus0);
            _ ->
                NewStatus0
        end,
    {ok, NewStatus1}.

handle_result_notify (Pid, {decide, Value},
                      Status = #status{ result=Result }) ->
    case result:add(Pid, Value, Result) of
        {error, _} ->
            {error, double_decision};
        {ok, NewResult} ->
            log_serv:decision_count(result:count(NewResult)),
            NewStatus = Status#status{
                result = NewResult
            },
            check_result(NewStatus)
    end.

handle_term_notify (Pid, _Ref, Reason,
                    Status = #status{ alive=Alive, stop=Stop } ) ->
    log_serv:log("Dead ~p: ~p", [Pid, Reason]),
    log_serv:node_count(gb_sets:size(Alive) - 1),
    NewStatus = Status#status {
        alive=gb_sets:delete_any(Pid, Alive)
    },
    case Stop of
        false -> check_result(NewStatus);
        true -> check_term(NewStatus)
    end.

handle_info (ready, Status) ->
    log_serv:log("Waited enough. Starting first round"),
    {ok, start_protocol(Status)}.

introduce_random (One, AllOther) ->
    case gb_sets:size(AllOther) of
        0 -> ok;
        N ->
            Other = lists:nth(random:uniform(N),
                              gb_sets:to_list(AllOther)),
            keeper_inject:introduce(One, Other)
    end.

check_result (Status = #status{ alive=Alive, result=Result }) ->
    case gb_sets:size(Alive) of
        0 ->
            log_serv:log("All nodes are dead!"),
            stop;
        NAlive ->
            LogResult =
                fun ({Val, Count}) ->
                    log_serv:log("\tValue ~p has been selected by ~p nodes",
                                 [Val, Count])
                end,
            case result:count(Result) of
                NAlive ->
                    stop_protocol(Status),
                    log_serv:log("Consensus (should has been) reached:"),
                    lists:foreach(LogResult, result:stats(Result)),
                    log_serv:log("Killing remaining processes..."),
                    killall(gb_sets:to_list(Alive)),
                    log_serv:log("Terminating."),
                    stop;
                N when N < NAlive ->
                    {ok, Status}
            end
    end.

check_term (Status = #status{ stop=true, alive=Alive }) ->
    case gb_sets:size(Alive) of
        0 -> stop;
        _ -> {ok, Status}
    end.

stop_protocol (Status) ->
    keeper_proto:disable_beacon(),
    Status.

start_protocol (Status) ->
    keeper_inject:bcast({cons, start}),
    Status.

prepare_protocol (Status = #status{ tbeacon=TBeacon,
                                    beaconwait=BeaconWait }) ->
    log_serv:log("Preparing protocol..."),
    NewStatus = assign_roles(Status),

    keeper_proto:enable_beacon(TBeacon),
    log_serv:log("Nodes started. Letting them know each other..."),
    timer:send_after(BeaconWait * TBeacon, self(), ready),
    NewStatus.

assign_roles (Status = #status{ alive=Alive, statpeers_ratio=R }) ->
    N = gb_sets:size(Alive),
    AliveList = gb_sets:to_list(Alive),

    % Assignment of numbers for round
    Assignment = lists:zip(lists:seq(1, N), AliveList),
    cons_inject_bcast({init, Assignment}),

    % Enabling for statistics retrieval
    NStatPeers = trunc(N * R),
    StatPeers = lists:map(fun (T) -> element(2, T) end,
                          lists:takewhile(fun ({I, _}) ->
                                              I =< NStatPeers
                                          end,
                                          Assignment)
                         ),
    lists:foreach(fun faildet_enable_stats/1, StatPeers),
    Status#status {
        statpeers=StatPeers
    }.

cons_inject_bcast (Msg) ->
    keeper_inject:bcast({cons, Msg}).

faildet_inject (To, Msg) ->
    keeper_inject:send(To, {faildet, Msg}).

faildet_enable_stats (To) ->
    faildet_inject(To, {stats_enabled, true}).

killall (Alive) ->
    lists:foreach(fun (P) -> exit(P, kill) end, Alive).
