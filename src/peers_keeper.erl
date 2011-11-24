-module(peers_keeper).
-author("Giovanni Simoni").
-export([start/0, start_link/0, notify_spawn/1, notify_death/1,
         start_protocol/1]).

-behavior(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

% -----------------------------------------------------------------------
% gen_server callbacks:
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

handle_cast ({node_spawn, _Pid}, State) ->
    % A new node spawned up
    {noreply, State};

handle_cast ({node_result, _Pid, _Result}, State) ->
    % A result has come
    {noreply, State};

handle_cast ({node_death, _Pid, _Reason}, State) ->
    % A node is dead
    {noreply, State}.

% -----------------------------------------------------------------------
% Internal logic
% -----------------------------------------------------------------------

node_send (Pid, Message) ->
    chan:send_direct(Pid, {mail, Message}).

node_meet (Pid, NewNeighbor) ->
    chan:send_direct(Pid, {meet, NewNeighbor}).

% -----------------------------------------------------------------------
% Interface
% -----------------------------------------------------------------------

start () ->
    gen_server:start({local, ?MODULE}, ?MODULE, nil, []).

start_link () ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, nil, []).

notify_spawn (Pid) ->
    gen_server:cast(?MODULE, {node_spawn, Pid}).

notify_death (Pid, Reason) ->
    gen_server:cast(?MODULE, {node_death, Pid, Reason}).

notify_result (Pid, Result) ->
    gen_server:cast(?MODULE, {node_result, Pid, Result}).

start_protocol (_NodesSpec) ->
    %lists:foreach(fun ({N, Module} start_nodes(N, Module) end, NodesSpec).
    ok.

