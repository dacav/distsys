-module(peer_ctrl).
-author("Giovanni Simoni").
-export([notify_spawn/0, notify_result/1, notify_term/0, notify_term/1]).

notify_spawn () ->
    erlang:send(keeper, {spawn, self()}).

notify_result (Result) ->
    erlang:send(keeper, {result, self(), Result}).

notify_term (Reason) ->
    erlang:send(keeper, {term, self(), Reason}).

notify_term () ->
    notify_term(normal).
