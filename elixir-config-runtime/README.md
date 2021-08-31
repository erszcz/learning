# ConfigRuntime

## That's how `Application.compile_env!` works

```
$ iex -S mix
Erlang/OTP 24 [erts-12.0.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit]

Interactive Elixir (1.11.2) - press Ctrl+C to exit (type h() ENTER for help)
>>> ConfigRuntime.hello
"hello compile value 1"
>>> Application.get_env(:config_runtime_test, :opt1)
"runtime value 1"
>>>
```
