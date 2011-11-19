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

+   Simulate a reliable yet delay-unbounded channel;

+   Replace simulated channel with a real network-based channel (so far,
    having similar characteristics in real life);

+   Make processes fail randomly.

About the last point, since a failing process is no longer influencing the
system with message, a good technique is making the faulty node crash just
before it sends a message. More details on crashes are provided in the
_Faulty processes_ section.

This principle justifies the design of a transmission system based on
filters.

### Implementation

+   The sending is managed by a server, which is in charge of keeping the
    list of filters to be applied to each communication. The gen_server
    OTP system will be used;

+   The process can communicate to the server by a synchronous call. This
    will allow the server to know the pid of the sender.

## Faulty processes

