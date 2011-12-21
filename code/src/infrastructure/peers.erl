-module(peers).
-author("Giovanni Simoni").

-behavior(supervisor).
-export([init/1, start_link/0]).

-define(MAX_RESTART, 3).
-define(MAX_TIME_RESTART, 1000).
-define(KILL_THRESHOLD, 1000).

-import(conf).

init (nil) ->
    % GO!
    io:format(standard_error, "Starting peers pool...~n", []),
    {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME_RESTART}, []}}.

start_link () ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, nil).

