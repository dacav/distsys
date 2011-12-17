-module(gfd_peer).
-author("Giovanni Simoni").
-behavior(gen_peer).
-export([init/1, handle_message/3, handle_introduction/3,
         handle_info/2, handle_beacon/1]).

-import(peer_chan).
-import(peer_ctrl).

-import(consensus).
-import(faildet).

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
handle_message (From, {cons, Msg}, Status = #status{ cons=Cons }) ->
    try
        NewCons =
            case Msg of
                {init, IdAssignment} ->
                    consensus:init(IdAssignment);
                _ ->
                    case consensus:handle_message(From, Msg, Cons) of
                        {ok, UpdCons} -> UpdCons;
                        Error -> throw(Error)
                    end
            end,
        Status#status{
            cons=NewCons
        }
    of
        S -> {ok, S}
    catch
        throw:E -> E
    end.

handle_introduction (From, Pid, Status = #status{ fd=FD, cons=nil }) ->
    % Used during the very first protocol phase, when the `cons` variable
    % is still to be built (before id assignment).
    try
        NewFD =
            case faildet:handle_introduction(From, Pid, FD) of
                {ok, FD0} -> FD0;
                E0 -> throw(E0)
            end,
        Status#status {
            fd=NewFD
        }
    of
        S -> {ok, S}
    catch
        throw:E -> E
    end;

handle_introduction (From, Pid, Status = #status{ fd=FD, cons=Cons }) ->
    try
        NewFD =
            case faildet:handle_introduction(From, Pid, FD) of
                {ok, FD0} -> FD0;
                E0 -> throw(E0)
            end,
        NewCons =
            case faildet:get_last_born(NewFD) of
                nil -> Cons;
                Born ->
                    NAlive = element(1, faildet:get_neighbors(NewFD)),
                    Msg = {born, Born, NAlive},
                    case consensus:handle_message(faildet, Msg, Cons) of
                        {ok, Cons0} -> Cons0;
                        E1 -> throw(E1)
                    end
            end,
        Status#status {
            fd=NewFD,
            cons=NewCons
        }
    of
        S -> {ok, S}
    catch
        throw:E -> E
    end.

handle_info (_Info, Status = #status{}) ->
    {ok, Status}.

handle_beacon (Status = #status{ fd=FD, cons=Cons }) ->
    try
        NewFD =
            case faildet:handle_beacon(FD) of
                {ok, FD0} -> FD0;
                E0 -> throw(E0)
            end,
        NewCons =
            case faildet:get_last_dead(NewFD) of
                [] -> Cons;
                Dead ->
                    NAlive = element(1, faildet:get_neighbors(NewFD)),
                    Msg = {dead, Dead, NAlive},
                    case consensus:handle_message(faildet, Msg, Cons) of
                        {ok, Cons1} -> Cons1;
                        E1 -> throw(E1)
                    end
            end,
        Status#status {
            fd=NewFD,
            cons=NewCons
        }
    of
        S -> {ok, S}
    catch
        thorw:E -> E
    end.
