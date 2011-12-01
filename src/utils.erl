-module(utils).
-author("Giovanni Simoni").
-export([startup/5]).

-define(TIMEOUT, 1000).

-import(log_serv).

init (Father, GenInit, Loop, Ref, NodeModule, NodeArgs) ->
    case GenInit() of
        ok ->
            case apply(NodeModule, init, NodeArgs) of
                {ok, NodeData} ->
                    erlang:send(Father, {Ref, ok}),
                    Loop(NodeModule, NodeData);
                {error, Reason} ->
                    erlang:send(Father, {Ref, error, Reason})
            end;
        {error, Reason} ->
            erlang:send(Father, {Ref, error, Reason})
    end.

startup (Spawner, GenInit, Loop, Module, Args) ->
    Me = self(),
    Ref0 = erlang:make_ref(),
    Pid = Spawner(fun () ->
                      init(Me, GenInit, Loop, Ref0, Module, Args)
                  end),
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
