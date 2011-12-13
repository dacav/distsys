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
    result=result:new(),

    % Initialization parameters, commented later
    npeers,
    tbeacon,
    beaconwait
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

init ({FDParams = {_,_,_}, NPeers, TBeacon, BeaconWait}) ->
    random:seed(now()), % Necessary for random peer introduction
    keeper_proto:add_peers(NPeers, gfd_peer, FDParams),
    Status = #status{
        npeers = NPeers,
        tbeacon = TBeacon,
        beaconwait = BeaconWait
    },
    {ok, Status}.

handle_spawn_notify (Pid, Status = #status{ alive=Alive, npeers=N }) ->
    log_serv:log("Started: ~p", [Pid]),
    % We introduce the new pid to a random process among the spawned ones.
    introduce_random(Pid, Alive),
    NewAlive = gb_sets:insert(Pid, Alive),
    NewStatus = Status#status{
        alive = NewAlive
    },
    case gb_sets:size(NewAlive) of
        N ->
            % Upon all nodes have been correctly spawned, we start thexs
            % protocol
            log_serv:log("All nodes started. Enabling protocol"),
            prepare_protocol(NewStatus);
        _ -> ok
    end,
    {ok, NewStatus}.

handle_result_notify (Pid, Value, Status = #status{ result=Result }) ->
    log_serv:log("Process ~p decided: ~p\n", [Pid, Value]),
    case result:add(Pid, Value, Result) of
        {error, _} ->
            {error, double_decision};
        {ok, NewResult} ->
            NewStatus = Status#status{
                result = NewResult
            },
            check_result(NewStatus)            
    end.

handle_term_notify (Pid, _Ref, Reason, Status = #status{ alive=Alive} ) ->
    log_serv:log("Dead: ~p (~p), ~p left",
                 [Pid, Reason, gb_sets:size(Alive) - 1]),
    NewStatus = Status#status {
        alive=gb_sets:delete(Pid, Alive)
    },
    check_result(NewStatus).

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
    LogResult =
        fun ({Val, Count}) ->
                log_serv:log("\tValue ~p has been selected by ~p nodes",
                             [Val, Count])
        end,
    NAlive = gb_sets:size(Alive),
    case result:count(Result) of
        NAlive ->
            stop_protocol(Status),
            log_serv:log("Consensus (should be) reached. Stats:"),
            lists:foreach(LogResult, result:stats(Result)),
            log_serv:log("End of statistics"),
            stop;
        N when N < NAlive ->
            {ok, Status}
    end.

stop_protocol (Status) ->
    keeper_proto:disable_beacon(),
    Status.

start_protocol (Status) ->
    keeper_inject:bcast({cons, start}),
    Status.

prepare_protocol (Status = #status{ tbeacon=TBeacon,
                                    beaconwait=BeaconWait }) ->
    assign_numbers(Status),
    keeper_proto:enable_beacon(TBeacon),
    timer:send_after(BeaconWait * TBeacon, self(), ready).

assign_numbers (#status{ alive=Alive }) ->
    N = gb_sets:size(Alive),
    Assignment = lists:zip(gb_sets:to_list(Alive), lists:seq(1, N)),
    [ cons_inject(Pid, {round_info, I, N}) || {Pid, I} <- Assignment ].

cons_inject (To, Msg) ->
    keeper_inject:send(To, {cons, Msg}).
