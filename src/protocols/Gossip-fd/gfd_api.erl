-module(gfd_api).
-author("Giovanni Simoni").
-export([faildet_send/2, cons_send/2]).

-import(peer_chan).
-import(peer_ctrl).

faildet_send (To, Msg) ->
    peer_chan:send(To, {faildet, Msg}).

cons_send (To, Msg) ->
    peer_chan:send(To, {cons, Msg}).

cons_bcast (Msg) ->
    peer_chan:bcast_send({cons, Msg}).

cons_decide (Val) ->
    Msg = {decide, Val},
    peer_chan:bcast_send(Msg),
    peer_ctrl:notify_result(Msg).
