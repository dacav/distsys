% Ensuring all is ready
make:all([load]).

% Staring up application
application:start(yuna).

% Declare killling for supervisor
gfd_keeper:schedule_killing([1]).
% Disable keeper persistance: it will give up if f < n/2
gfd_keeper:persist(false).

% Let the system start
gfd_keeper:launch_consensus().

% Syncrhonizing over keeper (keeper dies when consensus is reached)
MonRef = erlang:monitor(process, keeper),

gfd_keeper:persist(false),

receive
    {'DOWN', MonRef, process, _, noproc} ->
        io:format("Is this really happening?\n");

    {'DOWN', MonRef, process, _, normal} ->
        % The keeper is dead.

        % This is a synchronous call. Since it will be served in FIFO
        % order with respect to the log_serv message queue, we are sure
        % that all messages from the application are arrived
        log_serv:sync(),

        erlang:demonitor(MonRef, [flush]),
        erlang:exit(normal)
end.
