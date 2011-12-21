-module(gfd_api).
-author("Giovanni Simoni").
-export([faildet_send/2, faildet_greet/1, faildet_bcast/1,
         cons_send/2, cons_decide/1, cons_bcast/1]).

-import(peer_chan).
-import(peer_ctrl).

faildet_send (To, Msg) ->
    peer_chan:send(To, {faildet, Msg}).

faildet_greet (To) ->
    peer_chan:greet(To).

faildet_bcast (Msg) ->
    peer_chan:bcast_send({faildet, Msg}).

cons_send (To, Msg) ->
    peer_chan:send(To, {cons, Msg}).

cons_bcast (Msg) ->
    peer_chan:bcast_send({cons, Msg}).

cons_decide (Val) ->
    Msg = {decide, Val},
    %peer_chan:bcast_send({cons, Msg}),
    peer_ctrl:notify_result(Msg).
