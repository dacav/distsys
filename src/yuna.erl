-module(yuna).
-behavior(application).
-export([start/2, stop/1]).

-import(main).
-import(chan_filters).

build_closure ({M, F, A}) ->
    fun () -> apply(M, F, A) end.

set_channel () ->
    % Settings for: Faulty nodes
    {ok, FaultyProb} = application:get_env(yuna, faulty_prob),
    {ok, FaultyFailProb} = application:get_env(yuna, faulty_fail_prob),

    % Settings for: Random delay in delivery
    {ok, MinDelay} = application:get_env(yuna, deliver_mindel),
    {ok, MaxDelay} = application:get_env(yuna, deliver_maxdel),
    {ok, DelayDist} = application:get_env(yuna, deliver_dist),

    Filters = [
        chan_filters:hitman(FaultyProb * FaultyFailProb),
        chan_filters:random_deliver(MinDelay, MaxDelay,
                                    build_closure(DelayDist))
    ],

    application:set_env(yuna, chan_filters, Filters).

start (normal, _Args) ->
    io:format(standard_error, "Setting tweaked channel...~n", []),
    set_channel(),
    io:format(standard_error, "Starting YUNA...~n", []),
    main:start_link().

stop (_State) ->
    io:format(standard_error, "Terminating YUNA~n", []),
    ok.

