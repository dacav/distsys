-module(gfd_keeper).
-author("Giovanni Simoni").
-behavior(gen_keeper).
-export([init/1, handle_spawn_notify/2, handle_result_notify/3,
         handle_term_notify/4, handle_info/2]).

-import(keeper_inject).
-import(keeper_proto).

-record(status, {
    spawned,
    alive=gb_sets:new(),
    dead=gb_sets:new()
}).

init ({TFail, TCleanup, TGossip}) ->
    random:seed(now()),
    keeper_proto:enable_beacon(1000),
    PeerParam = {TFail, TCleanup, TGossip},
    Nodes = keeper_proto:add_peers_pidonly(5, gfd_peer,
                                           PeerParam),
    Status = #status{
        spawned = Nodes
    },
    {ok, Status}.

introduce_random (Last, AllOther) ->
    case gb_sets:size(AllOther) of
        0 -> ok;
        N ->
            Target = lists:nth(random:uniform(N),
                               gb_sets:to_list(AllOther)),
            keeper_inject:introduce(Target, Last),
            keeper_inject:introduce(Last, Target)
    end.

handle_spawn_notify (Pid, Status = #status{ alive=Alive }) ->
    log_serv:log("Started: ~p", [Pid]),
    introduce_random(Pid, Alive),
    NewStatus = Status#status{
        alive=gb_sets:insert(Pid, Alive)
    },
    {ok, NewStatus}.

handle_result_notify (_Pid, _Result, Status) ->
    {ok, Status}.

handle_term_notify (Pid, _Ref, Reason, Status = #status{ dead=Dead }) ->
    log_serv:log("Dead: ~p (~p)", [Pid, Reason]),
    NewStatus = Status#status {
        dead = gb_sets:insert(Pid, Dead)
    },
    {ok, NewStatus}.

handle_info (_Info, Status) ->
    {ok, Status}.
