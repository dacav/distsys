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
    dead=gb_sets:new(),
    npeers
}).

% Functioning logic for keeper:
%
% The keeper starts a certain number of peers (provided by the NPeers
% parameter). It each time a node is spawned the keeper introduces two
% peers.
%
% When all nodes have been spawned, the keeper starts the beacon: this
% will trigger the gossip algorithm running on the peers, which will
% allow node to know each other.

init ({TFail, TCleanup, TGossip, NPeers}) ->
    random:seed(now()),
    PeerParam = {TFail, TCleanup, TGossip},
    Nodes = keeper_proto:add_peers_pidonly(NPeers, gfd_peer, PeerParam),
    Status = #status{
        spawned = Nodes,
        npeers = NPeers
    },
    {ok, Status}.

introduce_random (One, AllOther) ->
    case gb_sets:size(AllOther) of
        0 -> ok;
        N ->
            Other = lists:nth(random:uniform(N),
                              gb_sets:to_list(AllOther)),
            keeper_inject:introduce(One, Other)
    end.

handle_spawn_notify (_, #status{ alive=all }) ->
    {error, tampered_protocol};
handle_spawn_notify (Pid,
                     Status = #status{ alive=Alive, npeers=NPeers }) ->
    log_serv:log("Started: ~p", [Pid]),
    introduce_random(Pid, Alive),
    NewAlive =
        case gb_sets:size(Alive) + 1 of
            NPeers ->
                log_serv:log("All nodes started. Enabling protocol"),
                keeper_proto:enable_beacon(1000),
                all;
            _ ->
                gb_sets:insert(Pid, Alive)
        end,
    NewStatus = Status#status{
        alive = NewAlive
    },
    {ok, NewStatus}.

handle_result_notify (_Pid, _Result, Status) ->
    {ok, Status}.

handle_term_notify (Pid, _Ref, Reason,
                    Status = #status{ dead=Dead }) ->
    log_serv:log("Dead: ~p (~p)", [Pid, Reason]),
    NewStatus = Status#status {
        dead = gb_sets:insert(Pid, Dead)
    },
    {ok, NewStatus}.

handle_info (_Info, Status) ->
    {ok, Status}.
