-module(peers_keeper).
-author("Giovanni Simoni").
-export([start/0, start_link/0, notify_spawn/1, notify_death/2,
         start_protocol/1]).

-behavior(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

-import(chan_infr).
-define(KILL_TIMEOUT, 1000).

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
    chan_infr:keeper_to_node(Pid, {mail, Message}).

node_meet (Pid, NewNeighbor) ->
    chan_infr:keeper_to_node(Pid, {meet, NewNeighbor}).

start_nodes (N, Module) ->
    Spec = {Module,
        {Module, start_link, []},
        transient, ?KILL_TIMEOUT, worker, dynamic
    },
    Pids = [supervisor:start_child(peers, Spec) || _ <- lists:seq(1, N)],
    {Module, Pids}.

start_protocol (NodesSpec) ->
    lists:map(fun ({N, Module}) -> start_nodes(N, Module) end, NodesSpec).

% -----------------------------------------------------------------------
% Interface
% -----------------------------------------------------------------------

% -----------------------------------------------------------------------
% Towards startup:

start () ->
    gen_server:start({local, ?MODULE}, ?MODULE, nil, []).

start_link () ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, nil, []).

% -----------------------------------------------------------------------
% Towards generic node (gen_node):

notify_spawn (Pid) ->
    gen_server:cast(?MODULE, {node_spawn, Pid}).

notify_death (Pid, Reason) ->
    gen_server:cast(?MODULE, {node_death, Pid, Reason}).

notify_result (Pid, Result) ->
    gen_server:cast(?MODULE, {node_result, Pid, Result}).

% -----------------------------------------------------------------------
% Towards specific node (chan_infr):

node_notification (Pid, Message) ->
    gen_server:cast(?MODULE, {node_notify, Pid, Message}).

