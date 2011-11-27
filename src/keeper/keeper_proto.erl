-module(keeper_proto).
-author("Giovanni Simoni").
-export([add_peers/3]).

build_spec (I, Module, PeerArg) ->
    {{peer, I},
     {gen_peer, start_link, [Module, PeerArg]},
     transient,
     brutal_kill,
     worker,
     dynamic
    }.

start_child (ChildSpec) ->
    supervisor:start_child(peers, ChildSpec).

add_peers (N, Module, PeerArgs) when is_number(N) andalso N > 0 ->
    Specs = [build_spec(I, Module, PeerArgs) || I <- lists:seq(1, N)],
    case supervisor:check_childspecs(Specs) of
        ok ->
            {ok, lists:map(fun start_child/1, Specs)};
        {error, Error} ->
            {error, Error}
    end.
