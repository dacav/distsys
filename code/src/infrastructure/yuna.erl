-module(yuna).
-behavior(application).
-export([start/2, stop/1]).

-import(main).
-import(chan_filters).

build_closure ({M, F, A}) ->
    fun () -> apply(M, F, A) end.

get_conf (Key) ->
    case application:get_env(yuna, Key) of
        {ok, Value} -> Value;
        undefined -> throw({configuration, Key})
    end.

set_conf (Key, Value) ->
    application:set_env(yuna, Key, Value).

conf_channel () ->
    % Settings for: Faulty nodes
    FaultyProb = get_conf(faulty_prob),
    FaultyFailProb = get_conf(faulty_fail_prob),

    % Settings for: Random delay in delivery
    MinDelay = get_conf(deliver_mindel),
    MaxDelay = get_conf(deliver_maxdel),
    DelayDist = get_conf(deliver_dist),

    Filters = [
        chan_filters:hitman(FaultyProb * FaultyFailProb),
        chan_filters:random_deliver(MinDelay, MaxDelay,
                                    build_closure(DelayDist))
    ],

    set_conf(chan_filters, Filters).

conf_keeper () ->
    Keeper = get_conf(keeper),
    KeeperArgs = get_conf(keeper_args),
    {Keeper, KeeperArgs}.

start (normal, _Args) ->
    io:format(standard_error, "Loading configuration...~n", []),
    try
        conf_channel(),
        {Keeper, KeeperArgs} = conf_keeper(),
        LogArgs = get_conf(log_args),
        io:format(standard_error, "Starting YUNA...~n", []),
        main:start_link(Keeper, KeeperArgs, LogArgs)
    catch
        throw:{configuration, Key} ->
            io:format(standard_error,
                      "ERROR: Bad configuration, missing '~p'~n",
                      [Key])
    end.

stop (_State) ->
    io:format(standard_error, "Terminating YUNA~n", []),
    ok.

