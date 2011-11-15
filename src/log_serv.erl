-module(log_serv).
-author("Giovanni Simoni").
-export([start/0, start/1, start_link/0, start_link/1, log/1, log/2]).

-behavior(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).


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

init (Args=[OutFile]) ->
    {ok, OutFile}.

print_date (OutFile) ->
    {{Y,M,D},{H,Mi,S}} = calendar:local_time(),
    io:format(OutFile, "~p/~p/~p ~p:~p:~p ", [Y,M,D,H,Mi,S]),
    ok.

handle_call ({Format, Data}, _, OutFile)
             when is_list(Format) and is_list(Data) ->
    io:format(OutFile, Format, Data),
    {reply, ok, OutFile};

handle_call (S, _, OutFile) ->
    Fmt = case is_list(S) of
          false -> "~p~n"; true -> "~s~n"
          end,
    print_date(OutFile),
    io:fwrite(OutFile, Fmt, [S]),
    {reply, ok, OutFile}.

start (OutFile) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [OutFile], []).

start_link (OutFile) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [OutFile], []).

start () ->
    start(standard_error).

start_link () ->
    start_link(standard_error).

log (Fmt, Args) ->
    gen_server:call(?MODULE, {Fmt, Args}).

log (S) ->
    gen_server:call(?MODULE, S).

