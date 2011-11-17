-module(services).
-author("Giovanni Simoni").

-behavior(supervisor).
-export([init/1, start_link/0]).

-define(TIMEOUT, 3000).
-define(MAX_RESTART, 3).
-define(MAX_TIME_RESTART, 1000).
-define(KILL_THRESHOLD, 1000).

-import(conf).

init (_) ->
    % Channel service descriptor
    Channel = {channel,
         {ranchan, start_link, conf:get_default(ranchan)},
         permanent, ?KILL_THRESHOLD, worker, [ranchan]
    },
    % Log service descriptor
    Logger = {logger,
                {log_serv, start_link, conf:get_default(log_serv)},
                permanent, ?KILL_THRESHOLD, worker, [log_serv]
    },
    % GO!
    {ok,
    	{{one_for_one, ?MAX_RESTART, ?MAX_TIME_RESTART},
         [Logger, Channel]
        }
  	}.

start_link () ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
