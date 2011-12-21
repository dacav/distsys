-module(bcast).
-author("Giovanni Simoni").
-export([start/0, start_link/0, subscribe/0, enable_beacon/1,
         disable_beacon/0]).

-behavior(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

-import(tweaked_chan).

-record(state, {beacon=infinity,
                subscribers=[]}).

% -----------------------------------------------------------------------
% Unused callbacks
% -----------------------------------------------------------------------

code_change (_, State, _) -> {ok, State}.
terminate (_, _) -> ok.
handle_cast (_, Subscs) -> {noreply, Subscs}.

% -----------------------------------------------------------------------
% Callbacks for gen_server
% -----------------------------------------------------------------------

handle_call (subscribe, {Pid, _}, State) ->
    build_reply_ok(
        add_subscriber(Pid, State)
    );

handle_call ({beacon, Timeout}, _, State) ->
    build_reply_ok(
        change_beacon(Timeout, State)
    ).

handle_info (timeout, State) ->
    send(bcast, State#state.subscribers, beacon),
    build_noreply(State);

handle_info ({From, Message}, State) when is_pid(From)
                                     orelse From =:= keeper ->
    send(From, State#state.subscribers, Message),
    build_noreply(State).

init (nil) ->
    {ok, #state{}}.

% -----------------------------------------------------------------------
% Server logic
% -----------------------------------------------------------------------

add_subscriber (NewPid, #state{beacon=B, subscribers=Ss}) ->
    #state{beacon=B, subscribers=[NewPid | Ss]}.

change_beacon (NewBeacon, #state{subscribers=Ss}) ->
    #state{beacon=NewBeacon, subscribers=Ss}.

send (From, Subscs, Msg) when is_list(Subscs) ->
    Send =
        fun (Pid) ->
            tweaked_chan:untweaked_send(From, Pid, Msg)
        end,
    lists:foreach(Send, Subscs).

build_reply_ok (State) ->
    {reply, ok, State, State#state.beacon}.

build_noreply (State) ->
    {noreply, State, State#state.beacon}.

% -----------------------------------------------------------------------
% Interface
% -----------------------------------------------------------------------

start () ->
    gen_server:start({local, ?MODULE}, ?MODULE, nil, []).

start_link () ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, nil, []).

subscribe () ->
    gen_server:call(?MODULE, subscribe).

enable_beacon (MilliSeconds) when MilliSeconds > 0 ->
    gen_server:call(?MODULE, {beacon, MilliSeconds}).

disable_beacon () ->
    gen_server:call(?MODULE, {beacon, infinity}).
