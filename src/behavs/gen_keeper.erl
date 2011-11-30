-module(gen_keeper).
-author("Giovanni Simoni").
-export([behaviour_info/1, start_link/2]).

-import(utils).
-import(log_serv).

behaviour_info(callbacks) -> [
    {init, 1},                  % params: Arg from start_link
    {handle_spawn_notify, 2},   % params: Pid, Arg
    {handle_result_notify, 3},  % params: Pid, Result, Arg
    {handle_term_notify, 4},    % params: Pid, Ref | undefined, Reason, Arg
    {handle_info, 2}            % params: Anything from down stack, Arg
];
behaviour_info(_) -> undefined.

loop (Module, Arg) ->
    KeeperReaction =
        receive
            {spawn, Pid} ->
                Module:handle_spawn_notify(Pid, Arg);
            {result, Pid, Result} ->
                Module:handle_result_notify(Pid, Result, Arg);
            {term, Pid, Reason} ->
                Module:handle_term_notify(Pid, undefined, Reason, Arg);
            {'DOWN', Ref, process, Pid, Reason} ->
                Module:handle_term_notify(Pid, Ref, Reason, Arg);
            Anything ->
                Module:handle_info(Anything, Arg)
        end,
    case KeeperReaction of
        {ok, UpdatedArg} ->
            loop(Module, UpdatedArg);
        {error, E} ->
            log_serv:log("Keeper is dead: ~p", [E]);
        stop ->
            log_serv:log("Keeper finished.")
    end.

start_link (Module, Args) ->
    Register = fun () -> erlang:register(keeper, self()), ok end,
    utils:startup(fun erlang:spawn_link/1, Register, fun loop/2,
                  Module, Args).
