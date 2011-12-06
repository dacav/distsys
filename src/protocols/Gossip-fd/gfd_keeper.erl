-module(gfd_keeper).
-author("Giovanni Simoni").
-behavior(gen_keeper).
-export([init/1, handle_spawn_notify/2, handle_result_notify/3,
         handle_term_notify/4, handle_info/2]).

-import(keeper_inject).
-import(keeper_proto).

-record(status, {waiting_for,
                 spawned,
                 npeers}).

init (_Arg) ->
    {ok, nil}.

handle_spawn_notify (_Pid, _Status) ->
    {ok, nil}.

handle_result_notify (_Pid, _Result, _Status) ->
    {ok, nil}.

handle_term_notify (_Pid, _Ref, _Reason, _Status) ->
    {ok, nil}.

handle_info (_Info, _Status) ->
    {ok, nil}.
