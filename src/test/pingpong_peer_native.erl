-module(pingpong_peer_native).
-author("Giovanni Simoni").
-behavior(gen_peer).
-export([init/1, handle_message/3, handle_introduction/3, handle_info/2,
         handle_beacon/1]).

-import(peer_chan).
-import(peer_ctrl).

init (Arg) ->
    io:format("Starting adventure\n"),
    case erlang:load_nif("./pingpong_peer_native", 0) of
        % XXX Tuple parameter: fullfills the assumptions
        ok -> init({Arg});
        {error, E, Txt} -> {error, {E, Txt}}
    end.

handle_message (_From, N, _Next) when is_number(N) ->
    {error, noload}.

handle_introduction (_From, _NewNode, _Next) ->
    {error, noload}.

handle_info (_Noise, _Next) ->
    {error, noload}.

handle_beacon (_Next) ->
    {error, noload}.
