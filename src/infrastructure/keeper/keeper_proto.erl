-module(keeper_proto).
-author("Giovanni Simoni").
-export([add_peers/3, add_peers_pidonly/3, enable_beacon/1,
         disable_beacon/0]).

-import(bcast).

build_spec (I, Module, Seed, PeerArg) ->
    {{peer, Module, I},
     {gen_peer, start_link, [Module, Seed, PeerArg]},
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

forge_seeds (0, _) -> [];
forge_seeds (N, PrevSeed) ->
    NextSeed = element(2, random:uniform_s(PrevSeed)),
    [{N, NextSeed} | forge_seeds(N - 1, NextSeed)].

add_peers (N, Module, PeerArg) when is_number(N) andalso N > 0 ->
    Specs = [build_spec(I, Module, Seed, PeerArg)
             || {I, Seed} <- forge_seeds(N, random:seed())
            ],
    case supervisor:check_childspecs(Specs) of
        ok ->
            lists:map(fun start_child/1, Specs);
        {error, Error} ->
            throw({wrong_child_specs, Error})
    end.
    % TODO: document. Yields:
    %   {ok, L} where L = [{Pid, MonitorRef}] in case of success
    %   {error, E} on fall

add_peers_pidonly (N, Module, PeerArg) when is_number(N)
                                            andalso N > 0 ->

    lists:map(fun(T) -> element(1, T) end, add_peers(N, Module, PeerArg)).

enable_beacon (MilliSeconds) when MilliSeconds > 0 ->
    bcast:enable_beacon(MilliSeconds).

disable_beacon () ->
    bcast:disable_beacon().
