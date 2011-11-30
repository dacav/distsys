-module(peer_chan).
-author("Giovanni Simoni").
-export([send/2, greet/1, bcast_send/1, bcast_greet/0]).

-import(tweaked_chan).

send (To, Msg) ->
    tweaked_chan:send(To, {mail, Msg}).

greet (OtherNode) ->
    tweaked_chan:send(OtherNode, {meet, self()}).

bcast_send (Msg) ->
    tweaked_chan:send(bcast, {mail, Msg}).

bcast_greet () ->
    tweaked_chan:send(bcast, {meet, self()}).
