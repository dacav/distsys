-module(services).
-author("Giovanni Simoni").

-behavior(supervisor).
-export([init/1, start_link/0]).

-define(MAX_RESTART, 3).
-define(MAX_TIME_RESTART, 1000).
-define(KILL_THRESHOLD, 1000).

-import(conf).

init (nil) ->
    % Log service descriptor
    Logger = {logger,
        {log_serv, start_link, conf:get_default(log_serv)},
        permanent, ?KILL_THRESHOLD, worker, [log_serv]
    },
    % Peers keeper server
    PeersKeeper = {peers_keeper,
        {peers_keeper, start_link, []},
        permanent, ?KILL_THRESHOLD, worker, [peers_keeper]
    },
    % GO!
    io:format(standard_error, "Starting services...~n", []),
    {ok,
    	{{one_for_one, ?MAX_RESTART, ?MAX_TIME_RESTART},
         [Logger, PeersKeeper]
        }
  	}.

start_link () ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, nil).

