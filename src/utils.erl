-module(utils).
-author("Giovanni Simoni").
-export([startup/4, startup/5]).

-define(TIMEOUT, 1000).

-import(log_serv).

init (Father, Loop, Ref, NodeModule, NodeArg) ->
    case apply(NodeModule, init, NodeArg) of
        {ok, NodeData} ->
            erlang:send(Father, {Ref, ok}),
            Loop(NodeModule, NodeData);
        {error, Reason} ->
            erlang:send(Father, {Ref, error, Reason})
    end.

startup (Spawner, Name, Loop, Module, Args) ->
    Me = self(),
    Ref0 = erlang:make_ref(),
    Pid = case Name of
          undefined -> Spawner(fun () ->
                               init(Me, Loop, Ref0, Module, Args)
                               end);
          _ -> Spawner(fun () ->
                       register(Name, self()),
                       init(Me, Loop, Ref0, Module, Args)
                       end)
          end,
    Ref1 = erlang:monitor(process, Pid),
    ToReturn =
        receive
            {'DOWN', Ref1, process, Pid, Reason} ->
                {error, Reason};
            {Ref0, ok} ->
                {ok, Pid};
            {Ref0, error, Reason} ->
                {error, Reason}
        after ?TIMEOUT ->
            {error, timeout}
        end,
    erlang:demonitor(Ref1, [flush]),
    ToReturn.

startup (Spawner, Loop, Module, Args) ->
    startup(Spawner, undefined, Loop, Module, Args). 
