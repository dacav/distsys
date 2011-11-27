-module(gen_peer).
-author("Giovanni Simoni").

-export([behaviour_info/1]).
-export([start_link/2]).

-import(utils).
-import(peer_ctrl).

behaviour_info(callbacks) -> [
    {init, 1},                  % params: Arg from start_link
    {handle_message, 3},        % params: From, To, PrivateData
    {handle_introduction, 3},   % params: From, NewPeer, PrivateData
    {handle_noise, 2}           % params: Noise, PrivateData
];
behaviour_info(_) -> undefined.

loop (NodeModule, NodeData) ->
    peer_ctrl:notify_spawn(),
    NodeReaction =
        receive
            {From, Msg} when is_pid(From)
                        orelse From =:= keeper ->
                case Msg of
                    {mail, Msg} ->
                        NodeModule:handle_message(From, Msg, NodeData);
                    {meet, Pid} ->
                        NodeModule:handle_introduction(From, Pid, NodeData)
                end;
            Anything ->
                NodeModule:handle_noise(Anything, NodeData)
        end,
    case NodeReaction of
        {ok, NodeUpdatedData} ->
            loop(NodeModule, NodeUpdatedData);
        {error, Reason} ->
            peer_ctrl:notify_death(Reason);
        stop ->
            peer_ctrl:notify_death(normal);
        {stop, Result} ->
            peer_ctrl:notify_result(Result),
            peer_ctrl:notify_death(normal)
    end.

start_link (Module, Args) ->
    utils:startup(fun erlang:spawn_link/1, fun loop/2, Module, Args).
