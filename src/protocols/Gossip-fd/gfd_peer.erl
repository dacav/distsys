-module(gfd_peer).
-author("Giovanni Simoni").
-behavior(gen_peer).
-export([init/1, handle_message/3, handle_introduction/3,
         handle_info/2, handle_beacon/1]).

-import(peer_chan).
-import(peer_ctrl).

-import(consensus).

-record(status, {
    fd,
    cons=nil
}).

init (FDParams = {_,_,_}) ->
    Status = #status{
        fd = faildet:init(FDParams)
    },
    peer_ctrl:notify_spawn(),
    {ok, Status}.

% ------------------------------------------------------------------------
% Messages for Failure Detector logic
% ------------------------------------------------------------------------

handle_message (From, {faildet, Msg}, Status = #status{ fd=FD }) ->
    case faildet:handle_message(From, Msg, FD) of
        {ok, NewFD} ->
            NewStatus = Status#status {
                fd=NewFD
            },
            {ok, NewStatus};
        Error -> Error
    end;
handle_message (From, {cons, Msg}, Status) ->
    log_serv:log("Message from ~p for consensus: ~p", [From, Msg]),
    {ok, Status}.

handle_introduction (From, Pid, Status = #status{ fd=FD }) ->
    case faildet:handle_introduction(From, Pid, FD) of
        {ok, NewFD} ->
            NewStatus = Status#status {
                fd=NewFD
            },
            {ok, NewStatus};
        Error -> Error
    end.

handle_info (Info, Status = #status{ fd=FD }) ->
    case faildet:handle_info(Info, FD) of
        {ok, NewFD} ->
            NewStatus = Status#status {
                fd=NewFD
            },
            {ok, NewStatus};
        Error -> Error
    end.

handle_beacon (Status = #status{ fd=FD }) ->
    case faildet:handle_beacon(FD) of
        {ok, NewFD} ->
            % HERE PROFIT
            NewStatus = Status#status {
                 fd=NewFD
            },
            {ok, NewStatus};
        Error -> Error
    end.
