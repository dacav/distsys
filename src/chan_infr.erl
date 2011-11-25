-module(chan_infr).
-author("Giovanni Simoni").
-export([keeper_to_node/2, node_to_keeper/1]).

-import(peers_keeper).

keeper_to_node (Pid, Msg) ->
    erlang:send(Pid, {keeper, Msg}).

node_to_keeper (Msg) ->
    peers_keeper:node_notification(self(), Msg).
