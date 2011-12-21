# YUNA Project

<pre>
Y U NO Agree? -- щ(ﾟДﾟщ)  
Distributed System Project  
Consensus Protocol
</pre>

### Note:

The project is still not working. This document is also not complete. It
will be updated as new parts of the project will be added.

## General structure

The application is compliant with the _Erlang/OTP_ design principle.
For the moment the tree is the following:

<pre>
yuna
   main
      services
         log_serv
         keeper
      peers
         P0
         P1
         ...
         Pn
</pre>

### Roles distribution

*   The `main` supervisor is in charge of managing the whole application
    (_oion strategy_, see Erlang documentation);

*   The `services` supervisor keeps alive the services needed by the
    system:

    +   `log_serv` is a server in charge of providing a logging system
        (see the *Logging* section);

    +   `keeper` is a process which:

        -   Is defined at application startup (configuration file);

        -   Corresponds to a user-defined module, henceforth `Module`
            which implements the _behavs/gen_keeper_ behavior
            (`behaves/gen_keeper.erl`);

        -   Can add nodes to a protocol (`keeper_proto:add_peers/3`);

        -   Is notified about node-related events happened during protocol
            execution (`Module:handle_spawn_notify/2`,
            `Module:handle_result_notify/3`,
            `Module:handle_term_notify/4`, `Module:handle_info/2`);

        -   Can tweak the protocol by injecting messages in nodes message
            queue, possibly with a spoofed pid (`keeper_inject:send/2,3`,
            `keeper_inject:introduce/2,3`).

*   The `peers` supervisor manages the peers, acting as pool.

    +   Peers are processes injected into the protocol by the `keeper`
        component.

    +   Each peer:

        -   Is defined by the `keeper` process logic (thus managed by the
            user);

        -   Corresponds to a user-defined module which implements the _peer_
            behavior (`behavs/peer.erl`);

        -   Is provided with an API which allows to communicate with other
            peers (`peer_chan:send/2`, `peer_chan:greet/1`), interact with
            the keeper (`peer_ctrl:notify_spawn/0`,
            `peer_ctrl:notify_result/1`, `peer_ctrl:notify_term/0,1`).

## Communication logic

Processes send data trough the underlying Erlang communication system (see
the Erlang documentation for `erlang:send/2`).

We want to be able to

*   Simulate a reliable yet delay-unbounded channel;

*   Replace simulated channel with a real network-based channel (so far,
    having similar characteristics in real life);

*   Make processes fail randomly.

About the last point, since a failing process is no longer influencing the
system with message, a good technique is making the faulty node crash just
before it sends a message. More details on crashes are provided in the
*Faulty processes* section.

This principle justifies the design of a transmission system based on
filters.

### Implementation

*   As initial design, the sending was managed by a server, the _channel
    manager_, which was in charge of keeping the list of filters to be
    applied to each communication.

    +   The `gen_server` OTP system was used.

    +   This design unfortunately prevents the process from being
        independent while sending, since they have to queue up before
        talking with the _channel manager_.

*   In order to avoid the bottleneck a more distributed approach has been
    adopted:

    +   During application startup the set of filters gets written in the
        global configuration

        -   *Note*: filters are built basing on the application
            configuration, which in turn can be overriden by
            external configuration)

    +   Under the hood, the `peer_chan:send/2` function is in charge of
        retrieving and applying the filters;

*   Drawbacks:

    +   As failure is a stochastic event, the behavior is dictated by the
        random generator. In _Erlang_, however, there's one PRNG is
        per-process, thus each process is forced to initialize the random
        generator in order to properly use this abstraction. This is now
        automatically achieved during startup of peers.

## Faulty processes

## Logging
