# timer_nif

An Erlang NIF library written in Rust

## Build

    $ make

## Test

    $ make test

## NIF thread sending messages to Erlang

    1> timer_nif:start_timer(), flush().
    Shell got {tick,1}
    ok
    2> flush().
    Shell got {tick,2}
    ok
    3> flush().
    Shell got {tick,3}
    Shell got {tick,4}
    Shell got {tick,5}
    Shell got {tick,6}
    ok
    4>

Once the thread reaches tick 3600, i.e. after one hour, it sends a final
`done` message.
