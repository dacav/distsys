-module(peers_keeper).
-author("Giovanni Simoni").
-export([start/0, start_link/0]).

-behavior(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

% -----------------------------------------------------------------------
% Unused callbacks
% -----------------------------------------------------------------------

code_change (_, State, _) ->
    {ok, State}.

handle_call (_, _, State) ->
    {reply, ok, State}.

handle_info (_, State) ->
    {noreply, State}.

terminate (_, _) ->
    ok.

init (nil) ->
    {ok, nil}.

handle_cast (_, State) ->
    {noreply, State}.

start () ->
    gen_server:start({local, ?MODULE}, ?MODULE, nil, []).

start_link () ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, nil, []).

