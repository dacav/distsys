-module(gfd_peer).
-author("Giovanni Simoni").
-behavior(gen_peer).
-export([init/1, handle_message/3, handle_introduction/3, handle_info/2,
         handle_beacon/1]).

-import(peer_chan).
-import(peer_ctrl).

-record(status, {
    fd,
})

init ({TFail, TCleanup, TGossip}) ->
    Status = #status{
        fd = faildet:new(TFail, TCleanup, TGossip),
    }
    {ok, Status}.

handle_message (_From, _Msg, _Prvt) ->
    {ok, nil}.

handle_introduction (_From, _Other, _Prvt) ->
    {ok, nil}.

handle_info (_Noise, _Prvt) ->
    {ok, nil}.

handle_beacon (Status = #status{ fd=FD }) ->
    NewFD = faildet:period(FD),
    NewFD = faildet:period(FD),
    NewStatus = Status#{
        fd = NewFD,
    }
    {ok, NewStatus}.
