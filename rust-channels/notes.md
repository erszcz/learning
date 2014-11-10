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
