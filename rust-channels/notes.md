# Erlang/OTP Supervisor Pattern in Rust

Core problem of implementing a supervisor and a service is the fact that
`Receiver` end of a channel must be uniquely owned.
This means that the supervisor and the service itself can't keep the copy
of the structure describing the service if the structure itself contains
a read end of a channel.

On the other hand, in order to be able to send data to the service,
it's necessary for it to have a `Receiver`.

The only reasonable solution seems to create the channel inside the service
itself and pass the `Sender` to the outside environment.

Requirements towards a `Supervisor`:

- it must be able to stop a `Service` from outside (call `.stop()`)
- it must be able to restart a `Service`

# 2014-11-11 More thoughts on Service interface

Two facts:

1. Channel `Sender` and `Receiver` endpoints are created with a single call
   to `channel()`.

2. `Receiver` must be uniquely owned -- no copies allowed.

This leads me to split the `Service` interface into two parts - `Service`
itself and `ServiceControls`.

The first is used to start the service (most often by a supervisor).
Inside the `start` function the initial state of the service
is created - presumably channels it will use to communicate with
the outside world - and the service's `serve()` is called.
It turned out that `serve()` doesn't have to be a part of any of the
interfaces involved - how to start the service inside `start()` is completely
up to the implementer - using `serve()` is just a suggestion.

The second interface is `ServiceControls`, intended to wrap
the `Sender` endpoints of any channels created inside the start function
of the service.
The only required function in `ServiceControls` is `stop()` used by the
supervisor to stop the service, though given it's the only interface which
can use channels to communicate with the task of the other service,
it's `ServiceControls` which should implement all the custom functionality
of the service in question.
