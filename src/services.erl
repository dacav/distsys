-module(services).
-author("Giovanni Simoni").

-behavior(supervisor).
-export([init/1, start_link/2]).

-define(MAX_RESTART, 3).
-define(MAX_TIME_RESTART, 1000).
-define(KILL_THRESHOLD, 1000).

-import(conf).

init ([Keeper, KeeperArgs]) ->
    % Log service descriptor
    Logger = {logger,
        {log_serv, start_link, conf:get_default(log_serv)},
        permanent, ?KILL_THRESHOLD, worker, [log_serv]
    },
    % Peers keeper server
    PeersKeeper = {peers_keeper,
        {gen_keeper, start_link, [Keeper, KeeperArgs]},
        transient, ?KILL_THRESHOLD, worker, [peers_keeper]
    },
    %
    BroadCast = {bcast,
        {bcast, start_link, []},
        permanent, ?KILL_THRESHOLD, worker, [bcast]
    },
    % GO!
    io:format(standard_error, "Starting services...~n", []),
    {ok,
    	{{one_for_one, ?MAX_RESTART, ?MAX_TIME_RESTART},
         [Logger, BroadCast, PeersKeeper]
        }
  	}.

start_link (Keeper, KeeperArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE,
                          [Keeper, KeeperArgs]).
