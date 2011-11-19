-module(chan).
-author("Giovanni Simoni").
-export([start/1, start_link/1, send/2]).

-behavior(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

-behavior(conf).
-export([default_conf/0]).

% -----------------------------------------------------------------------
% Unused callbacks
% -----------------------------------------------------------------------

code_change (_, State, _) ->
    {ok, State}.

handle_cast (_, State) ->
    {noreply, ok, State}.

handle_info (_, State) ->
    {noreply, State}.

terminate (_, _) ->
    ok.

% -----------------------------------------------------------------------
% Server logic
% -----------------------------------------------------------------------

init (Filters) ->
    {ok, Filters}.

% This function assumes the message with its envelope, namely a tuple
% {To, From, Msg}.
%
% Filters are functions which can either manipulate the message or send
% it. In the first case they have to return a new message with envelope,
% and the next filter will be executed; in the second case they have to
% return the sent message itself.
%
% If no filter sends the data, a regular sending will be achieved.
%
filter_apply (ConfirmTo, From, To, Msg, Filters) ->
    case Filters of
        [] ->
            erlang:send(To, {From, Msg}),
            confirm(ConfirmTo);
        [Flt | Flts] -> case Flt(From, To, Msg) of
                            {F,T,M} -> filter_apply(ConfirmTo, F, T, M, Flts);
                            Msg -> confirm(ConfirmTo);
                            _ -> throw({badarg, invalid_filter})
                        end
    end.

confirm (To) ->
    gen_server:reply(To, ok).

handle_call ({send, To, Msg}, ConfirmTo = {From, _Ref}, Filters) ->
    spawn(fun () -> filter_apply(ConfirmTo, From, To, Msg, Filters) end),
    {noreply, Filters}.

% -----------------------------------------------------------------------
% Interface
% -----------------------------------------------------------------------

start (Filters) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Filters, []).

start_link (Filters) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Filters, []).

send (To, Msg) ->
    gen_server:call(?MODULE, {send, To, Msg}).

% -----------------------------------------------------------------------
% Default Configuration
% -----------------------------------------------------------------------

default_conf () -> [

].

