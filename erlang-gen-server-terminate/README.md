# erlang-gen-server-terminate

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

Case 10 might be surprising, but it actually makes perfect sense.
See `man erlang` for the documentation of `exit/2`: _If Reason is the atom `normal`, Pid does not exit._
This is due to the fact that an `exit(normal)` call is a valid clean termination of a process.
However, each call to `exit/1` sends an exit signal to linked processes - in order not to kill
linked processes upon normal termination, `normal` exit reason is treated specially by `exit/2`.
It's also described at http://erlang.org/doc/reference_manual/processes.html

## ToDo

With and without trapping exits:
- `gen_server` shutdown by supervisor
- `gen_server` shutdown by parent
