-module(yuna).
-behavior(application).
-export([start/2, stop/1]).

-import(services).

start (normal, _Args) ->
    io:format("Initializing random seed...~n"),
    io:format("Starting YUNA!~n"),
    services:start_link().

stop (State) ->
    io:format("Terminating YUNA (State=~p)~n", [State]),
    ok.

