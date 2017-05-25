# 2017-05-25


## Can't export ADT "constructors" for use from Erlang

As of now, there's no way to provide type hints to Alpaca.
This means the following variation of `basic_pid_test` won't work:

```
let m_add i = Add i

let m_fetch pid = Fetch (pid)
```

The above fails with an exhaustiveness check.
This is (probably) due to the fact that `pid` in general can be different
than `pid int` from `Fetch (pid int)`.

EDIT: Not true! We can, but it's not as simple as it could be - see
`src/basic_pid_test.alp` for implementation and below for usage example:

```erlang
$ rebar3 shell
===> Verifying dependencies...
===> Compiling example
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:0] [kernel-poll:false]

Eshell V8.2  (abort with ^G)
1>
1>
1> Proc = alpaca_basic_pid_test:start_pid_fun(3).
<0.707.0>
2> Proc ! alpaca_basic_pid_test:m_fetch(self()).
{'Fetch',<0.704.0>}
3> flush().
Shell got 3
ok
4> Proc ! alpaca_basic_pid_test:m_add(5).
{'Add',5}
5> Proc ! alpaca_basic_pid_test:m_fetch(self()).
{'Fetch',<0.704.0>}
6> flush().
Shell got 8
ok
7>
```
