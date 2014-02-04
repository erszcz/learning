# `netcat` talking to Erlang

Shell 1:

    $ cat > test
    ala ma kota, a kot ma ale
    $ nc -l 1234 < test

Shell 2:

    > {ok, Socket} = gen_tcp:connect("localhost", 1234, [binary, {active, once}]).
    {ok,#Port<0.636>}
    > flush().
    Shell got {tcp,#Port<0.636>,<<"ala ma kota, a kot ma ale\n">>}
    ok
    > inet:setopts(Socket, [{active, once}]).
    ok
    > flush().
    Shell got {tcp_closed,#Port<0.636>}
    ok
