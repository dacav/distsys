-module(main).
-author("Giovanni Simoni").

-behavior(supervisor).
-export([init/1, start_link/0]).

-define(MAX_RESTART, 3).
-define(MAX_TIME_RESTART, 1000).
-define(KILL_THRESHOLD, 1000).

-import(conf).

init (nil) ->
    % Log service descriptor
    Services = {services,
        {services, start_link, []},
        permanent, infinity, supervisor, [services]
    },
    % GO!
    Peers = {peers,
        {peers, start_link, []},
        permanent, infinity, supervisor, [peers]
    },
    io:format(standard_error, "Starting main supervisor...~n", []),
    {ok,
    	{{one_for_one, ?MAX_RESTART, ?MAX_TIME_RESTART},
         [Services, Peers]
        }
  	}.

start_link () ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, nil).

