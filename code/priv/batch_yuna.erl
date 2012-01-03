% Ensuring all is ready
make:all([load]).

% Staring up application
application:start(yuna).

% Syncrhonizing over keeper (keeper dies when consensus is reached)
MonRef = erlang:monitor(process, keeper),
receive
    X ->
        % This is a synchronous call. Since it will be served in FIFO
        % order with respect to the log_serv message queue, we are sure
        % that all messages from the application are arrived
        log_serv:sync(),

        erlang:demonitor(MonRef, [flush]),
        erlang:exit(normal)
end.
