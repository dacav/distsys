-module(respawn).
-export([start/0, start/1, run/2, kill/1, term/2, report/0]).
-define(TIMEOUT, 1000).

% ------------------------------------------------------------------------
% Respawning logic
% ------------------------------------------------------------------------

proc_spawn_register (Atom, Function) ->
    Pid = spawn_link(Function),
    register(Atom, Pid),
    Pid.

% If the atom is not registered, register it with the given id, else throw
% exception.
proc_start (Atom, Function) ->
    case whereis(Atom) of
        undefined -> proc_spawn_register(Atom, Function);
        P -> throw( {exists, Atom, P} )
    end.

proc_exit (Pid, How) ->
    unlink(Pid),
    exit(Pid, How).

% ------------------------------------------------------------------------
% Functions for management of active processes, mapping pids to pairs
% atoms and functions, where functions are the entry points for the
% processes they represent.
%
% The mapping is achieved by balanced trees, however this detail is
% abstracted away.
% ------------------------------------------------------------------------

ap_empty () -> gb_trees:empty().

ap_exited (Pid, Tree) ->
    case is_process_alive(Pid) of
        true -> throw(tampered);    % process was not dead? wtf?
        false -> gb_trees:delete(Pid, Tree)
    end.

ap_crashed (Pid, Active) ->
    {Atom, Fun} = gb_trees:get(Pid, Active),
    gb_trees:insert(proc_start(Atom, Fun), {Atom, Fun},
                    ap_exited(Pid, Active)).

ap_spawn (Atom, Fun, Tree) ->
    Pid = proc_start(Atom, Fun),
    gb_trees:insert(Pid, {Atom, Fun}, Tree).

ap_exit (Atom, Tree, How) ->
    Pid = whereis(Atom),
    proc_exit(Pid, How),
    gb_trees:delete(Pid, Tree).

ap_report (Active) ->
    lists:map(fun ({P,{A,_}}) -> {A,P} end, gb_trees:to_list(Active)).

ap_pid2atom (Pid, Active) ->
    element(1, gb_trees:get(Pid, Active)).

% ------------------------------------------------------------------------
% Main loop
% ------------------------------------------------------------------------

loop (Active, undefined) ->
    GetAtom = fun (P) -> ap_pid2atom(P, Active) end,
    receive
        {'EXIT', Pid, normal} ->
            log ! {"Process ~p exited normally", [GetAtom(Pid)]},
            loop( ap_exited(Pid, Active), undefined);
        {'EXIT', Pid, Reason} ->
            log ! {"Respawning ~p (died by '~p')", [GetAtom(Pid), Reason]},
            loop( ap_crashed(Pid, Active), undefined );
        {From, {run, Atom, Fun}} when is_pid(From) ->
            try ap_spawn(Atom, Fun, Active) of
                X ->
                    log ! {"New process: ~p", [Atom]},
                    loop(X, {From, ok})
            catch
                throw:{exists, Atom, _} ->
                    log ! {"Attempt to build ~p twice", [Atom]},
                    loop(Active, {From, error})
            end;
        {From, {kill, Atom}} when is_pid(From) ->
            log ! {"Killing ~p", [Atom]},
            loop( ap_exit(Atom, Active, kill), {From, ok} );
        {From, {term, Atom, How}} when is_pid(From) ->
            log ! {"Shutting down ~p", [Atom]},
            loop( ap_exit(Atom, Active, How), {From, ok} );
        {From, report} when is_pid(From) ->
            loop( Active, {From, ap_report(Active)} );
        X ->
            log ! {"No match for ~p", [X]},
            loop( Active, undefined )
    end;
loop (Active, {To, Msg}) ->
    To ! Msg,
    loop(Active, undefined).

% ------------------------------------------------------------------------
% Communication with the respawn process
% ------------------------------------------------------------------------

ask_action (Act) ->
    try
        respawn ! {self(), Act},
        receive X -> X
        after ?TIMEOUT -> dead end
    catch
        error:badarg -> error   % process cannot be reached
    end.

% ------------------------------------------------------------------------
% Interface primitives
% ------------------------------------------------------------------------

run (Atom, Fun) when is_function(Fun) ->
    ask_action({run, Atom, Fun}).

kill (Atom) when is_atom(Atom) ->
    ask_action({kill, Atom}).

term (Atom, How) when is_atom(Atom) ->
    ask_action({term, Atom, How}).

report () ->
    ask_action(report).

% ------------------------------------------------------------------------
% Startup system
% ------------------------------------------------------------------------

main (Log) ->
    process_flag(trap_exit, true),
    Active =
        try
            ap_spawn(log, Log, ap_empty())
        catch
            throw:{exists, log, _} -> ap_empty()
        end,
    loop(Active, undefined).

start (Log) ->
    try
        proc_start(respawn, fun () -> main(Log) end), ok
    catch
        throw:{exists, respawn, _} -> error
    end.

default_log () ->
    receive
        X -> {{Y,M,D},{H,Mi,S}} = calendar:local_time(),
             io:format("~p/~p/~p ~p:~p:~p ", [Y,M,D,H,Mi,S]),
             case X of
                {Format, Data} -> io:format(standard_error, Format, Data);
                S when is_list(S) -> io:fwrite(standard_error, "~s", [S]);
                X -> io:fwrite(standard_error, "~p", [X])
             end,
             default_log()
    end.

start () ->
    start(fun () -> default_log() end).
