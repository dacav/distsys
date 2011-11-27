-module(pingpong_peer).
-author("Giovanni Simoni").
-behavior(gen_peer).
-export([init/1, handle_message/3, handle_introduction/3,
         handle_noise/2]).

init (Arg) ->
    io:format("I'm alive! (~p, Arg=~p)", [self(), Arg]),
    {ok, nil}.

handle_message (_From, _To, _Prvt) ->
    {ok, nil}.

handle_introduction (_From, _NewNode, _Prvt) ->
    {ok, nil}.

handle_noise (_Noise, _Prvt) ->
    {ok, nil}.
