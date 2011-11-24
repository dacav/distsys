-module(node).
-author("Giovanni Simoni").

-export([behaviour_info/1, start_link/2, start/2]

% Move in node API
-export([send/2, greet/1]).

-define(TIMEOUT, 5000).

% -----------------------------------------------------------------------
% Defining behavior for implementing nodes.
% -----------------------------------------------------------------------

behaviour_info (callbacks) -> [
        {init, 1},
        {got_message, 3},
        {got_noise, 2},
        {meet, 3}
    ];
behaviour_info (_) -> undefined.

init (Father, Ref, NodeModule, NodeArg) ->
    case NodeModule:init(NodeArg) of
        {ok, NodeData} ->
            peers_keeper:notify_spawn(self()),
            erlang:send(Father, {Ref, ok}),
            loop(self(), NodeModule, NodeData);
        {error, Reason} ->
            erlang:send(Father, {Ref, error, Reason})
    end.

loop (NodeId, NodeModule, NodeData) ->
    NodeReaction =
        receive
            {From, Msg} when is_pid(From)
                        orelse From =:= keeper ->
                case Msg of
                    {mail, Msg} ->
                        NodeModule:got_message(From, Msg, NodeData);
                    {meet, Pid} ->
                        NodeModule:meet(From, Pid, NodeData)
                end;
            Anything ->
                NodeModule:got_noise(Anything, NodeData)
        end,
    case NodeReaction of
        {ok, NodeUpdatedData} ->
            loop(NodeId, NodeModule, NodeUpdatedData);
        {error, Reason} -> Reason
            peers_keeper:notify_death(NodeId, Reason);
        stop ->
            peers_keeper:notify_death(NodeId, normal);
        {stop, Result} ->
            peers_keeper:notify_result(NodeId, Result);
            peers_keeper:notify_death(NodeId, normal)
    end.

% -----------------------------------------------------------------------
% Commands available for Nodes (TODO: move in node API)
% -----------------------------------------------------------------------

send (To, Msg) ->
    chan:send(To, {mail, Msg}).

greet (OtherPid) ->
    chan:send(OtherPid, {meet, self()}).

notify_result (Result) ->
    peers_keeper:notify_result(self(), Result).

% -----------------------------------------------------------------------
% Interface
% -----------------------------------------------------------------------

manage_startup (Spawner, Module, Args) ->
    Ref0 = erlang:make_ref(),
    Pid = Spawner(fun () -> init(self(), Ref0, Module, Args) end),
    Ref1 = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref1, process, Pid, Reason} -> {error, Reason};
        {Ref0, ok} -> {ok, Pid};
        {Ref0, error, Reason} -> {error, Reason}
    after ?TIMEOUT ->
        {error, timeout}
    end.

start (Module, Arg) ->
    manage_startup(fun erlang:spawn/1, Module, Arg).

start_link (Module, Arg) ->
    manage_startup(fun erlang:spawn_link/1, Module, Arg).

