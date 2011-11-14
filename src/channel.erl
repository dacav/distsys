-module(channel).
-author("Giovanni Simoni").
-export([start/2, start_link/2, start/3, start_link/3]).

-behavior(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

-export([send/2]).
-import(randel, [send_rand/5]).

% -----------------------------------------------------------------------
% Unused callbacks
% -----------------------------------------------------------------------

code_change (_, State, _) ->
    {ok, State}.

handle_cast (_, State) ->
    {noreply, State}.

handle_info (_, State) ->
    % TODO: Log inconsistent behavior.
    {noreply, State}.

terminate (_, _) ->
    % TODO: Log termination
    ok.

% -----------------------------------------------------------------------
% Server logic
% -----------------------------------------------------------------------

-record(params, {min_del, max_del, dist}).

init ([MinDelay, MaxDelay, Dist]) ->
    InitState = #params{min_del=MinDelay,
                        max_del=MaxDelay,
                        dist=Dist},
    {ok, InitState}.

handle_call ({send, To, Msg}, {From, _}, State) ->
    send_rand( State#params.min_del,
               State#params.max_del,
               State#params.dist,
               To, {From, Msg} ),
    {reply, ok, State}.

start (MinDelay, MaxDelay, Dist) ->
    ArgList = [MinDelay, MaxDelay, Dist],
    gen_server:start({local, ?MODULE}, ?MODULE, ArgList, []).

start_link (MinDelay, MaxDelay, Dist) ->
    ArgList = [MinDelay, MaxDelay, Dist],
    gen_server:start_link({local, ?MODULE}, ?MODULE, ArgList, []).

start (MinDelay, MaxDelay) ->
    start (MinDelay, MaxDelay, fun random:uniform/0).

start_link (MinDelay, MaxDelay) ->
    start_link (MinDelay, MaxDelay, fun random:uniform/0).

send (To, Msg) ->
    gen_server:call(?MODULE, {send, To, Msg}). 

