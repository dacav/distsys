-module(peer_chan).
-author("Giovanni Simoni").
-export([send/2, greet/1]).

-import(tweaked_chan).

send (To, Msg) ->
    tweaked_chan:send(To, {mail, Msg}).

greet (OtherNode) ->
    tweaked_chan:send(OtherNode, {meet, self()}).
