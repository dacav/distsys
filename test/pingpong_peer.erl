-module(pingpong_peer).
-author("Giovanni Simoni").
-behavior(gen_peer).
-export([init/1, handle_message/3, handle_introduction/3, handle_info/2]).

-import(peer_chan).
-import(peer_ctrl).

init (Arg) ->
    log_serv:log("I'm alive! (~p, Arg=~p)", [self(), Arg]),
    peer_ctrl:notify_spawn(),
    {ok, nil}.

handle_message (From, N, Next) when is_number(N) ->
    log_serv:log("Homework from ~p: N=~p", [From, N]),
    case N of
        0 -> peer_ctrl:notify_result(finish);
        _ -> peer_chan:send(Next, N - 1)
    end,
    {ok, Next};

handle_message (From, Msg, Next) ->
    log_serv:log("Message from ~p saying ~p", [From, Msg]),
    {ok, Next}.

handle_introduction (From, NewNode, Next) ->
    log_serv:log("Introduction from ~p. Setting next: from ~p to ~p",
                 [From, Next, NewNode]),
    peer_chan:send(NewNode, "Oh hi!"),
    {ok, NewNode}.

handle_info (Noise, Next) ->
    log_serv:log("Got noise: ~p", [Noise]),
    {ok, Next}.
