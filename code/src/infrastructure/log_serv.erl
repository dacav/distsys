-module(log_serv).
-author("Giovanni Simoni").
-export([start/2, start_link/2, log/1, log/2,
         node_count/1, est_node_count/1, decision_count/1, event/1]).

-behavior(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

-record(loginfo, {
        outfile,
        statfds,
        starttime
    }
).

-record(statfiles, {
        node,
        est_node,
        decision,
        events
    }
).

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

now_millisec () ->
    {Mega, Sec, Micro} = erlang:now(),
    ((Mega * 1000000) + Sec) * 1000 + trunc(Micro / 1000).

openfile (Name, Suffix, Default) ->
    FN = Name ++ "_" ++ atom_to_list(Suffix) ++ ".log",
    case file:open(FN, write) of
        {ok, IODev} ->
            IODev;
        {error, Reason} ->
            io:format(Default, "Cannot open ~s (~p) defaulting to ~p",
                      [Default, Reason, Default]),
            Default
    end.

get_descriptor (SFDs, Class) ->
    case Class of
        node -> SFDs#statfiles.node;
        est_node -> SFDs#statfiles.est_node;
        decision -> SFDs#statfiles.decision;
        events -> SFDs#statfiles.events;
        _ -> erlang:error(no_class, [SFDs, Class])
    end.

init ({OutFile, StatFN}) ->
    % I hope erlang:time() is enough. It gives the time since 00:00:00
    % of the current day.
    StatFiles = #statfiles{
        node=openfile(StatFN, node_count, OutFile),
        est_node=openfile(StatFN, est_node_count, OutFile),
        decision=openfile(StatFN, decision_count, OutFile),
        events=openfile(StatFN, events, none)
    },
    Start = now_millisec(),
    {ok, #loginfo{ outfile=OutFile,
                   statfds=StatFiles,
                   starttime=Start
         }
    }.

print_head (Who, OutFile) ->
    {{Y,M,D},{H,Mi,S}} = calendar:local_time(),
    io:format(OutFile, "~p/~p/~p ~p:~p:~p ~p  ", [Y,M,D,H,Mi,S,Who]),
    ok.

timediff (#loginfo{ starttime=Start }) ->
    now_millisec() - Start.

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

handle_cast ({stat, Class, N}, Status = #loginfo{ statfds=SFDs }) ->
    T = timediff(Status),
    FD = get_descriptor(SFDs, Class),
    io:format(FD, "~p ~p\n", [T, N]),
    {noreply, Status};

handle_cast ({event, Name}, Status = #loginfo{ statfds=SFDs }) ->
    T = timediff(Status),
    case get_descriptor(SFDs, events) of
        none -> ok;
        FD ->
            io:format(FD, "set arrow from ~p,0 to ~p,1000 nohead\n", [T, T]),
            io:format(FD, "set label \"~s\" at ~p,0 center\n", [Name, T])
    end,
    {noreply, Status}.

start (OutFile, StatFN) ->
    gen_server:start({local, ?MODULE}, ?MODULE,
                     {OutFile, StatFN}, []).

start_link (OutFile, StatFN) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          {OutFile, StatFN}, []).

log (Fmt, Args) ->
    gen_server:cast(?MODULE, {text, self(), Fmt, Args}).

log (S) ->
    gen_server:cast(?MODULE, {text, self(), S}).

node_count (N) ->
    gen_server:cast(?MODULE, {stat, node, N}).

est_node_count (N) ->
    gen_server:cast(?MODULE, {stat, est_node, N}).

decision_count (N) ->
    gen_server:cast(?MODULE, {stat, decision, N}).

event (Name) ->
    gen_server:cast(?MODULE, {event, Name}).
