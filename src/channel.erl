-module(channel).
-author("Giovanni Simoni").
-export([start/2, start/3, send/2]).

-import(randel, [send_rand/5]).
-import(respawn).

% ------------------------------------------------------------------------
% Random delay transmission channel.
%
% When started propagation parameters (minimum delay, maximum delay and
% probability distribution over delay) can be provided. By spawning a
% permanent process (trough the 'respawn' module) we avoid carrying the
% channel parameters for each call.
% ------------------------------------------------------------------------

loop (MinDelay, MaxDelay, Dist) ->
    receive
        {From, To, Msg} ->
            send_rand(MinDelay, MaxDelay, Dist, To, {From, Msg})
    end,
    loop(MinDelay, MaxDelay, Dist).

start (MinDelay, MaxDelay, Dist) ->
    respawn:start(),
    respawn:run(chan, fun () -> loop(MinDelay, MaxDelay, Dist) end).

start (MinDelay, MaxDelay) ->
    start(MinDelay, MaxDelay, fun () -> random:uniform() end).

send (To, Msg) ->
    try
        chan ! {self(), To, Msg}
    catch
        error:badarg -> {error, not_started}
    end.

