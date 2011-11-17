-module(ranchan).
-author("Giovanni Simoni").
-export([start/1, start_link/1, send/2]).
-import(randel, [send_rand/3]).

-behavior(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

-behavior(conf).
-export([default_conf/0]).

-define(DEFAULT_MIN_DELAY, 0).
-define(DEFAULT_MAX_DELAY, 1000).
-define(DEFAULT_DELAY_DIST, fun random:uniform/0).

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

init (RandSpec) ->
    {ok, RandSpec}.

handle_call ({send, To, Msg}, {From, _}, RandSpec) ->
    send_rand(RandSpec, To, {From, Msg}),
    {reply, ok, RandSpec}.

% -----------------------------------------------------------------------
% Interface
% -----------------------------------------------------------------------

start (RandSpec) ->
    gen_server:start({local, ?MODULE}, ?MODULE, RandSpec, []).

start_link (RandSpec) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, RandSpec, []).

send (To, Msg) ->
    gen_server:call(?MODULE, {send, To, Msg}).

% -----------------------------------------------------------------------
% Default Configuration
% -----------------------------------------------------------------------

default_conf () ->
    randel:build_spec(?DEFAULT_MIN_DELAY, ?DEFAULT_MAX_DELAY,
                      ?DEFAULT_DELAY_DIST).

