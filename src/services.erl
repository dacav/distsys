-module(services).
-author("Giovanni Simoni").

-behavior(supervisor).
-export([init/1, start_link/1]).

-define(TIMEOUT, 3000).
-define(MAX_RESTART, 3).
-define(MAX_TIME_RESTART, 1000).
-define(KILL_THRESHOLD, 1000).

-record(servconf, {chan, logger}).

init (Params) ->
    % Channel service descriptor
    Channel = {channel,
         {rand_chan, start_link, Params#servconf.chan},
         permanent, ?KILL_THRESHOLD, worker, [rand_chan]
    },
    % Log service descriptor
    Logger = {logger,
                {log_serv, start_link, Params#servconf.logger},
                permanent, ?KILL_THRESHOLD, worker, [log_serv]
    },
    % GO!
    {ok,
    	{{one_for_one, ?MAX_RESTART, ?MAX_TIME_RESTART},
         [Channel, Logger]
        }
  	}.

start_link (Params) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Params).
