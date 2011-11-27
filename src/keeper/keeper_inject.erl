-module(keeper_inject).
-author("Giovanni Simoni").
-export([send/2, send/3, introduce/1, introduce/2]).

-import(tweaked_chan).

send (To, Msg) ->
    tweaked_chan:untweaked_send(keeper, To, {mail, Msg}).

send (SpoofFrom, To, Msg) ->
    tweaked_chan:untweaked_send(SpoofFrom, To, {mail, Msg}).

introduce (OtherNode) ->
    tweaked_chan:send(keeper, {meet, OtherNode}).

introduce (SpoofFrom, OtherNode) ->
    tweaked_chan:send(SpoofFrom, {meet, OtherNode}).
