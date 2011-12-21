-module(gen_peer).
-author("Giovanni Simoni").

-export([behaviour_info/1]).
-export([start_link/3]).

-import(utils).
-import(peer_ctrl).
-import(bcast).

behaviour_info(callbacks) -> [
    {init, 1},                  % params: Arg from start_link
    {handle_message, 3},        % params: From, To, PrivateData
    {handle_introduction, 3},   % params: From, NewPeer, PrivateData
    {handle_beacon, 1},         % params: PrivateData
    {handle_info, 2}            % params: Info, PrivateData
];
behaviour_info(_) -> undefined.

loop (NodeModule, NodeData) ->
    NodeReaction =
        receive
            {From, Msg} when is_pid(From)
                        orelse From =:= keeper
                        orelse From =:= bcast ->
                case Msg of
                    {mail, Content} ->
                        NodeModule:handle_message(From, Content, NodeData);
                    {meet, Friend} ->
                        NodeModule:handle_introduction(From, Friend,
                                                       NodeData);
                    beacon ->
                        NodeModule:handle_beacon(NodeData);
                    Anything ->
                        NodeModule:handle_info({From, Anything}, NodeData)
                end;
            Anything ->
                NodeModule:handle_info(Anything, NodeData)
        end,
    case NodeReaction of
        {ok, NodeUpdatedData} ->
            loop(NodeModule, NodeUpdatedData);
        {error, Reason} ->
            peer_ctrl:notify_term(Reason);
        stop ->
            peer_ctrl:notify_term(normal);
        {stop, Result} ->
            peer_ctrl:notify_result(Result),
            peer_ctrl:notify_term(normal);
        Unknown ->
            throw({whatis, Unknown})
    end.

start_link (Module, Seed, Arg) ->
    % This will initialize the PRNG on peers (used for random
    % delay/kill during transmission).
    Init =
        fun () ->
            random:seed(Seed),
            bcast:subscribe(),
            ok
        end,
    utils:startup(fun erlang:spawn_link/1, Init, fun loop/2,
                  Module, Arg).
