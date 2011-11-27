-module(pingpong_keeper).
-author("Giovanni Simoni").
-behavior(gen_keeper).
-export([init/1, handle_spawn_notify/2, handle_result_notify/3,
         handle_term_notify/3]).

init (NPeers) ->
    io:format("Hi there, this is pingpong_keeper initalizing...\n"),
    Peers = keeper_proto:add_peers(NPeers, pingpong_peer, [nil]),
    log_serv:log("Initialized: ~p", [Peers]),
    {ok, Peers}.

handle_spawn_notify (Pid, Peers) ->
    log_serv:log("Started: ~p", [Pid]),
    {ok, Peers}.

handle_result_notify (Pid, Result, Peers) ->
    log_serv:log("Result from ~p: ~p~n", [Pid, Result]),
    {ok, Peers}.

handle_term_notify (Pid, Reason, Peers) ->
    log_serv:log("Terminated ~p: ~p~n", [Pid, Reason]),
    {ok, Peers}.
