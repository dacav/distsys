-module(gfd_api).
-author("Giovanni Simoni").
-export([faildet_send/2, cons_send/2]).

faildet_send (To, Msg) ->
    peer_chan:send(To, {faildet, Msg}).

cons_send (To, Msg) ->
    peer_chan:send(To, {cons, Msg}).
