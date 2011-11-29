-module(keeper_inject).
-author("Giovanni Simoni").
-export([send/2, send/3, introduce/2, introduce/3]).

-import(tweaked_chan).

send (To, Msg) ->
    tweaked_chan:untweaked_send(keeper, To, {mail, Msg}).

send (SpoofFrom, To, Msg) ->
    tweaked_chan:untweaked_send(SpoofFrom, To, {mail, Msg}).

introduce (To, OtherNode) ->
    tweaked_chan:untweaked_send(keeper, To, {meet, OtherNode}).

introduce (SpoofFrom, To, OtherNode) ->
    tweaked_chan:untweaked_send(SpoofFrom, To, {meet, OtherNode}).
