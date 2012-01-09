-module(gfd_keeper).
-author("Giovanni Simoni").
-behavior(gen_keeper).
-export([init/1, handle_spawn_notify/2, handle_result_notify/3,
         handle_term_notify/4, handle_info/2, persist/1,
         launch_consensus/0, schedule_killing/1]).

-import(keeper_inject).
-import(keeper_proto).

-import(result).

-record(status, {
    alive=gb_sets:new(),  % Set of spawned nodes
    result=result:new(),  % Results from node (stats)

    % Initialization parameters, commented later
    npeers,
    tbeacon,
    beaconwait,

    statpeers_ratio,
    statpeers=[],

    % Persist even if f > n/2
    persist=true,

    % Permission of starting: both must be true, the first is setted to
    % true when all nodes are spawned, the second may be true by default
    % (see the 'init' function) or gets true when 'launch_consensus'
    % gets been called.
    permission = {false, false},

    % Processes to be killed just after protocol startup. Initially a list
    % of Ids (like [1, 2, 3]), than, after the 'assign_roles' phase, a
    % list of Pids (like [<X,Y,Z> ...])
    tokill = []
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

init ({FDParams = {_,_,_}, NPeers, StartImmediately, StatPeers, TBeacon,
      BeaconWait}) ->
    random:seed(now()), % Necessary for random peer introduction
    keeper_proto:add_peers(NPeers, gfd_peer, FDParams),
    Status = #status{
        npeers = NPeers,
        tbeacon = TBeacon,
        beaconwait = BeaconWait,
        statpeers_ratio = StatPeers,
        permission = {false, StartImmediately}
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
        case {NAlive, Status#status.permission} of
            {N, {_, true}} ->
                % Upon all nodes have been correctly spawned, we start the
                % protocol
                log_serv:log("Nodes started: N=~p", [N]),
                prepare_protocol(NewStatus0#status{permission={true, true}});
            {N, {_, false}} ->
                log_serv:log("Waiting user"),
                NewStatus0#status{ permission={true, false} };
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

handle_term_notify (Pid, _Ref, Reason, Status = #status{ alive=Alive }) ->
    case Reason of
        crashed ->
            ok;
        _ ->
            log_serv:log("Dead ~p: ~p", [Pid, Reason])
    end,
    NewAlive = gb_sets:delete_any(Pid, Alive),
    log_serv:node_count(gb_sets:size(NewAlive)),
    check_result(Status#status{ alive=NewAlive }).

handle_info (ready, Status= #status{ tokill=DeadGuys }) ->
    log_serv:log("Ready! Killing to-be-killed and starting first round"),
    killall(DeadGuys),
    {ok, start_protocol(Status#status{ tokill=[] })};

handle_info ({persist, V}, Status) ->
    log_serv:log("Changing persistence"),
    {ok, Status#status{ persist=V }};

handle_info (launch, Status = #status{permission=P}) ->
    NewStatus = 
        case P of
            {_, true} ->
                log_serv:log("Double"),
                % Double call? Was already ok.
                Status;
            {true, _} ->
                log_serv:log("Go"),
                % All nodes already there, they're waiting for me.
                prepare_protocol(Status#status{permission={true, true}});
            {false, _} ->
                log_serv:log("Waiting spawn"),
                % Not all nodes are ready, I'm ready though.
                Status#status{permission={false, true}}
        end,
    {ok, NewStatus};

handle_info ({schedule_killing, SeqNumbers}, Status) ->
    {ok, Status#status{ tokill=SeqNumbers }}.

introduce_random (One, AllOther) ->
    case gb_sets:size(AllOther) of
        0 -> ok;
        N ->
            Other = lists:nth(random:uniform(N),
                              gb_sets:to_list(AllOther)),
            keeper_inject:introduce(One, Other)
    end.

check_result (Status = #status{ alive=Alive, result=Result, npeers=N }) ->
    case {gb_sets:size(Alive), result:count(Result)} of
        {A, _} when A < trunc(N/2) ->
            log_serv:log("Less than N/2 (~p) nodes are still alive", [A]),
            log_serv:event("f > n/2"),
            case Status#status.persist of
                true -> {ok, Status};
                false -> stop
            end;
        {X, X} ->
            LogResult =
                fun ({Val, Count}) ->
                    log_serv:log("\tValue ~p has been selected by ~p nodes",
                                 [Val, Count])
                end,
            stop_protocol(Status),
            log_serv:log("Consensus (should) has been reached:"),
            lists:foreach(LogResult, result:stats(Result)),
            log_serv:log("Killing remaining processes..."),
            killall(gb_sets:to_list(Alive)),
            log_serv:log("Terminating."),
            stop;
        _ ->
            {ok, Status}
    end.

stop_protocol (Status) ->
    keeper_proto:disable_beacon(),
    log_serv:event("stop protocol"),
    Status.

start_protocol (Status) ->
    log_serv:event("start protocol"),
    keeper_inject:bcast({cons, start}),
    Status.

prepare_protocol (Status = #status{ tbeacon=TBeacon,
                                    beaconwait=BeaconWait }) ->
    log_serv:log("Preparing protocol..."),
    log_serv:log("Permission=~p", [Status#status.permission]),
    NewStatus = assign_roles(Status),

    keeper_proto:enable_beacon(TBeacon),
    log_serv:log("Nodes started. Letting them know each other..."),
    timer:send_after(BeaconWait * TBeacon, self(), ready),
    NewStatus.

id_to_pid (Assignment, ToKill) ->
    Mapping = lists:foldl(fun ({I, P}, T) -> gb_trees:insert(I, P, T) end,
                          gb_trees:empty(), Assignment),
    L = [ gb_trees:lookup(I, Mapping) || I <- ToKill ],
    [ element(2, X) || X <- lists:filter( fun (X) -> X =/= none end, L ) ].

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
        statpeers=StatPeers,
        tokill=id_to_pid(Assignment, Status#status.tokill)
    }.

% ------------------------------------------------------------------------
% Protocol-complying communication
% ------------------------------------------------------------------------

cons_inject_bcast (Msg) ->
    keeper_inject:bcast({cons, Msg}).

faildet_inject (To, Msg) ->
    keeper_inject:send(To, {faildet, Msg}).

faildet_enable_stats (To) ->
    faildet_inject(To, {stats_enabled, true}).

killall (Alive) ->
    lists:foreach(fun (P) -> exit(P, kill) end, Alive).

% ------------------------------------------------------------------------
% Needed for testing
% ------------------------------------------------------------------------

persist (V) ->
    erlang:send(keeper, {persist, V}).

% Syncrhonize launching (do not launch until this method has been called)
launch_consensus () ->
    erlang:send(keeper, launch).

% Require killing of a certain list of processes based on their id
% assignment in the consensus protocol. For instance, in order to kill the
% very first coordinator SeqNumbers must be [1].
schedule_killing (SeqNumbers) when is_list(SeqNumbers) ->
    erlang:send(keeper, {schedule_killing, SeqNumbers}).
