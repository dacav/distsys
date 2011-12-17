-module(consensus).
-author("Giovanni Simoni").

-behavior(gen_peer).
-export([init/1, handle_message/3, handle_introduction/3,
         handle_info/2, handle_beacon/1]).

-import(gfd_api).

-record(cons, {
    id_assign,             % Assignments of IDs from coordinator;
    round=0,               % Current round for this node;
    phase=0,               % Phase of the round;
    est='?',               % Current estimation;
    est_from_c=nil,        % Estimation from coordinator (propagated);
    nalive,                % Currently alive nodes (according to FD);
    rec,                   % Set of results aggregated during phase2;
    prop,                  % Nodes who propagated the aggregated result
                           % during phase 2.
    decided=false
}).

% PHASES OF THE ALGORITHM
%
% Due to the software construction, a peer is seen as a set of callbacks
% corresponding to events. This requires the implemented algorithm to be
% splitted in more phases, each of which can be reached by subsequent
% incoming events.
%
% Regardless of the phase, every communication coming from the Failure
% Detector (namely notifications of newly appeared or disappeared nodes)
% corresponds to an updating of the corresponding status variable `nalive`.
%
% Phase 0 - Startup:
%    If the node is the current round coordinator, the estimation is
%    achieved (trough the `decide/0` function) and broadcasted to all
%    other nodes. In any case we go to Phase 1.
%
% Phase 1 - Waiting for something to happen:
%    Possible cases
%    *   A message carrying the estimation from the coordinator arrives
%        (thus we move to phase 2);
%    *   A message from the Failure Detector declares the coordinator as
%        faulty (thus we move to phase 0 of next round)
%
% Phase 2 - Waiting the response of at least N/2 nodes
%    At this point both a set of reporting nodes and reported values must
%    be kept. This phases last until the size of the reporting nodes set
%    is strictly smaller than trunc(N/2), so we also need to keep track of
%    changes in the number of nodes (information coming from the Failure
%    Detector.
%
% TODO Complete

init (IdAssignment) ->
    Tree = lists:foldl(fun ({I,P}, T) -> gb_trees:insert(I, P, T) end,
                       gb_trees:empty(), IdAssignment),
    #cons{ id_assign = Tree }.

handle_message (_From, _Msg, Cons = #cons{ decided=true }) ->
    % Ignoring everything, I've decided.
    {ok, Cons};

handle_message (keeper, start, Cons = #cons{ phase=0 }) ->
    run_round(Cons);

handle_message (faildet, {born, _BornList, NAlive}, Cons = #cons{}) ->
    % When the Failure-Detector signals some born node, the number of alive
    % peers must be updated.
    NewCons = Cons#cons{ nalive=NAlive },
    case NewCons#cons.phase of
        2 -> agreement(NewCons);
        _ -> {ok, NewCons}
    end;

handle_message (faildet, {dead, DeadList, NAlive}, Cons = #cons{}) ->
    % When the Failure-Detector signals some dead node, the number of
    % alive peers must be updated
    NewCons = Cons#cons{ nalive=NAlive },
    case {NewCons#cons.phase, is_coordinator_dead(NewCons, DeadList)} of
        % We are in phase 1 and the coordinator is dead. Go to phase 2
        % and set the estimate to '?'.
        {1, true} ->
            log_serv:log("Coordinator is dead"),
            run_phase2(NewCons#cons{ est_from_c='?' });
        {2, _} ->
            log_serv:log("Trying agreement"),
            agreement(NewCons);
        _ ->
            {ok, NewCons}
    end;

handle_message (_From, {est_c, Est_c}, Cons = #cons{ phase=1 }) ->
    % The massage from current coordinator (either direct or propagated by
    % other nodes) moves us to phase 2
    log_serv:log("Got estimation: ~p", [Est_c]),
    run_phase2(Cons#cons{ est_from_c=Est_c });

handle_message (From, {phase2, E},
                Cons = #cons{ phase=Phase, rec=Rec, prop=Prop }) ->
    case Phase of
        2 ->
            NewCons0 = Cons#cons {
                rec=ordsets:add_element(E, Rec),
                prop=gb_sets:add_element(From, Prop)
            },
            agreement(NewCons0);
        _ ->
            log_serv:log("Got a 'phase=2' message, discarded"),
            {ok, Cons}
    end;

handle_message (From, {decide, Value}, Cons = #cons{}) ->
    case self() of
        From ->
            {error, 'wtf?'};   % This should never happpen!
        _ ->
            gfd_api:cons_decide(Value),
            {ok, Cons#cons{ decided=true }}
    end.

handle_info (_Info, Cons = #cons{}) -> {ok, Cons}.
handle_beacon (Cons = #cons{}) -> {ok, Cons}.
handle_introduction (_From, _Pid, Cons) -> {ok, Cons}.

guess () ->
    % Decide true or false with same probability.
    random:uniform() < 0.5.

is_coordinator_dead (Cons, DeadList) ->
    Self = self(),
    case get_coordinator(Cons) of
        {_, Self} ->
            false;
        {_, CoordPid} ->
            IsCoord = fun (P) -> P =:= CoordPid end,
            lists:any(IsCoord, DeadList)
    end.

get_coordinator (#cons{ id_assign=Assignment, round=Round }) ->
    N = gb_trees:size(Assignment),
    Id = (Round rem N) + 1,
    {Id, gb_trees:get(Id, Assignment)}.

run_phase2 (Cons = #cons{ est_from_c=E }) ->
    gfd_api:cons_bcast({phase2, E}),
    NewCons = Cons#cons {
        phase=2,
        rec=ordsets:new(),
        prop=gb_sets:new()
    },
    {ok, NewCons}.

agreement (Cons=#cons{ phase=2, rec=Rec, prop=Prop, nalive=N }) ->
    GetVal = fun () -> ordsets:fold(fun (X,_) -> X end, ok, Rec) end,
    case gb_sets:size(Prop) of
        M when M < trunc(N/2) ->
            log_serv:log("I heard ~p nodes, waiting for other ~p (N=~p)",
                         [M, trunc(N/2), N]),
            {ok, Cons};
        _ ->
            log_serv:log("Rec=~p", [Rec]),
            case Rec of
                [_] -> % Rec = {v}
                    % Decide
                    log_serv:log("Deciding"),
                    gfd_api:cons_decide(GetVal()),
                    {ok, Cons#cons { decided=true }};
                ['?'] -> % Rec = {?}
                    log_serv:log("Next round"),
                    run_next_round(Cons);
                ['?', _] -> % Rec = {v, ?}
                    log_serv:log("Next round with value"),
                    NewCons = Cons#cons {
                        est=GetVal()
                    },
                    run_next_round(NewCons);
                _ ->
                    log_serv:log("Y U No Agree? Rec=~p", [Rec]),
                    {error, yuna}
            end
    end.

run_next_round (Cons = #cons{ round=Round }) ->
    run_round(
        Cons#cons{
            rec=undefined,
            prop=undefined,
            phase=0,
            round=Round + 1
        }
    ).

run_round (Cons = #cons{ est=Est }) ->
    % If a node recognizes itself as the coordinator, the estimated
    % decision value is broadcasted. If the value has not been estimated
    % yet, it's created randomly.
    Self = self(),
    NewEst =
        case get_coordinator(Cons) of
            {_, Self} ->
                Est_c =
                    case Est of
                        '?' -> guess();
                        _ -> Est
                    end,
                log_serv:log("Coordinator started with est=~p",
                             [Est_c]),
                gfd_api:cons_bcast({est_c, Est_c});
            {I, P} ->
                log_serv:log("Coordinator is ~p (~p)", [P, I]),
                Est
        end,
    NewCons = Cons#cons {
        est=NewEst,
        phase=1
    },
    {ok, NewCons}.
