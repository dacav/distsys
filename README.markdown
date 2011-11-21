# YUNA Project

Y U NO Agree? -- щ(ﾟДﾟщ)  
Distributed System Project  
Consensus Protocol

### Note:

The project is still not working. This document is also not complete. It
will be updated as new parts of the project will be added.

## Communication logic

Processes send data trough the underlying Erlang communication system (see
the Erlang documentation: `erlang:send/2`).

We want to be able to

*   Simulate a reliable yet delay-unbounded channel;

*   Replace simulated channel with a real network-based channel (so far,
    having similar characteristics in real life);

*   Make processes fail randomly.

About the last point, since a failing process is no longer influencing the
system with message, a good technique is making the faulty node crash just
before it sends a message. More details on crashes are provided in the
_Faulty processes_ section.

This principle justifies the design of a transmission system based on
filters.

### Implementation

*   As initial design, the sending was managed by a server, the `channel
    manager`, which was in charge of keeping the list of filters to be
    applied to each communication.

    +   The gen_server OTP system was used.

    +   This design unfortunately prevents the process from being
        independent while sending, since they have to queue up before
        talking with the `channel manager`.

*   In order to avoid the bottleneck a more distributed approach has been
    adopted:

    +   During application startup the set of filters gets written in the
        global configuration
        
        -   *Note*: filters are built basing on the application
            configuration, which in turn can be overriden by
            external configuration)
        
    +   Under the hood, the `chan:send/2` function is in charge of
        retrieving and applying the filters;

*   Drawbacks:

    +   As failure is a stochastic event, the behavior is dictated by the
        random generator. In _Erlang_, however, there's one PRNG is
        per-process, thus each process is forced to initialize the random
        generator in order to properly use this abstraction.

## Faulty processes
