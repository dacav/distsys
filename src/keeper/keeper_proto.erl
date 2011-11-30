-module(keeper_proto).
-author("Giovanni Simoni").
-export([add_peers/3, enable_beacon/1, disable_beacon/0]).

-import(bcast).

build_spec (I, Module, PeerArg) ->
    {{peer, I},
     {gen_peer, start_link, [Module, PeerArg]},
     temporary,
     brutal_kill,
     worker,
     dynamic
    }.

start_child (ChildSpec) ->
    case supervisor:start_child(peers, ChildSpec) of
        {ok, Pid} -> {Pid, erlang:monitor(process, Pid)};
        {error, E} -> throw({cannot_spawn, E})
    end.

add_peers (N, Module, PeerArgs) when is_number(N) andalso N > 0 ->
    Specs = [build_spec(I, Module, PeerArgs) || I <- lists:seq(1, N)],
    case supervisor:check_childspecs(Specs) of
        ok -> {ok, lists:map(fun start_child/1, Specs)};
        {error, Error} -> throw({wrong_child_specs, Error})
    end.

enable_beacon (MilliSeconds) when MilliSeconds > 0 ->
    bcast:enable_beacon (MilliSeconds).

disable_beacon () ->
    bcast:disable_beacon ().
