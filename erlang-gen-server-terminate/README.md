erlang-gen-server-terminate
=====

Test when `gen_server:terminate` will really be called:

```
$ rebar3 shell
> driver:test().
op: stop        trap: true      alive: false    terminate: called
op: crash       trap: true      alive: false    terminate: called
op: external_normal_exit        trap: true      alive: true     terminate: not_called
op: external_abnormal_exit      trap: true      alive: true     terminate: not_called
op: linked_process_exits        trap: true      alive: true     terminate: not_called
op: linked_process_crashes      trap: true      alive: true     terminate: not_called
op: stop        trap: false     alive: false    terminate: called
op: crash       trap: false     alive: false    terminate: called
op: external_normal_exit        trap: false     alive: true     terminate: not_called
op: external_abnormal_exit      trap: false     alive: false    terminate: not_called
op: linked_process_exits        trap: false     alive: true     terminate: not_called
op: linked_process_crashes      trap: false     alive: false    terminate: not_called
[ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok]
```
