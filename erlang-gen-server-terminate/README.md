erlang-gen-server-terminate
=====

Test when `gen_server:terminate` will really be called:

```
$ rebar3 shell
> driver:test().
  1) trap:  true        op:                   stop      alive: false    terminate:     called
  2) trap:  true        op:                  crash      alive: false    terminate:     called
  3) trap:  true        op:   external_normal_exit      alive:  true    terminate: not_called
  4) trap:  true        op: external_abnormal_exit      alive:  true    terminate: not_called
  5) trap:  true        op:      external_shutdown      alive:  true    terminate: not_called
  6) trap:  true        op:   linked_process_exits      alive:  true    terminate: not_called
  7) trap:  true        op: linked_process_crashes      alive:  true    terminate: not_called
  8) trap: false        op:                   stop      alive: false    terminate:     called
  9) trap: false        op:                  crash      alive: false    terminate:     called
 10) trap: false        op:   external_normal_exit      alive:  true    terminate: not_called
 11) trap: false        op: external_abnormal_exit      alive: false    terminate: not_called
 12) trap: false        op:      external_shutdown      alive: false    terminate: not_called
 13) trap: false        op:   linked_process_exits      alive:  true    terminate: not_called
 14) trap: false        op: linked_process_crashes      alive: false    terminate: not_called
[ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok]
```
