-module(yuna).
-behavior(application).
-export([start/2, stop/1]).

-import(services).

start (normal, Args) ->
    io:format("Starting YUNA (~p)~n", [Args]),
    services:start_link().

stop (State) ->
    io:format("Terminating YUNA (State=~p)~n", [State]),
    ok.

