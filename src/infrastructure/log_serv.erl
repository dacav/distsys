-module(log_serv).
-author("Giovanni Simoni").
-export([start/0, start/1, start_link/0, start_link/1, log/1, log/2,
         node_count/1, est_node_count/1, decision_count/1]).

-behavior(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

-behavior(conf).
-export([default_conf/0]).

-record(loginfo, {
        outfile,
        statfile,
        starttime
    }
).

% -----------------------------------------------------------------------
% Default Configuration: write on standard error
% -----------------------------------------------------------------------

default_conf () ->
    standard_error.

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

% -----------------------------------------------------------------------
% Server logic
% -----------------------------------------------------------------------

init (OutFile) ->
    % I hope erlang:time() is enough. It gives the time since 00:00:00
    % of the current day.
    Start = calendar:time_to_seconds(erlang:time()),
    {ok, #loginfo{ outfile=OutFile,
                   statfile=OutFile,
                   starttime=Start
         }
    }.

print_head (Who, OutFile) ->
    {{Y,M,D},{H,Mi,S}} = calendar:local_time(),
    io:format(OutFile, "~p/~p/~p ~p:~p:~p ~p  ", [Y,M,D,H,Mi,S,Who]),
    ok.

timediff (#loginfo{ starttime=Start }) ->
    calendar:time_to_seconds(erlang:time()) - Start.

handle_cast ({text, Who, Format, Data},
             Status = #loginfo{ outfile=OutFile })
        when is_list(Format) and is_list(Data) ->
    Output = try io_lib:format(Format, Data)
             catch error:badarg -> io_lib:format("~p~p", [Format, Data])
             end,
    print_head(Who, OutFile),
    io:format(OutFile, "~s~n", [Output]),
    {noreply, Status};

handle_cast ({text, Who, S}, Status = #loginfo{ outfile=OutFile }) ->
    Fmt = case is_list(S) of
          false -> "~p~n"; true -> "~s~n"
          end,
    print_head(Who, OutFile),
    io:fwrite(OutFile, Fmt, [S]),
    {noreply, Status};

handle_cast ({stat, Class, N}, Status = #loginfo{ statfile=SF }) ->
    T = timediff(Status),
    io:fwrite(SF, "~p:~p:~p\n", [Class, T, N]),
    {noreply, Status}.

start (OutFile) ->
    gen_server:start({local, ?MODULE}, ?MODULE, OutFile, []).

start_link (OutFile) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, OutFile, []).

start () ->
    OutFile = default_conf(),
    start(OutFile).

start_link () ->
    OutFile = default_conf(),
    start_link(OutFile).

log (Fmt, Args) ->
    gen_server:cast(?MODULE, {text, self(), Fmt, Args}).

log (S) ->
    gen_server:cast(?MODULE, {text, self(), S}).

node_count (N) -> ok.
    %gen_server:cast(?MODULE, {stat, node, N}).

est_node_count (N) -> ok.
    %gen_server:cast(?MODULE, {stat, est_node, N}).

decision_count (N) -> ok.
    %gen_server:cast(?MODULE, {stat, decided, N}).
