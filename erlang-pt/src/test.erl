-module(test).

-compile([export_all,
          {parse_transform, pt_echo}]).

start() ->
    this_is_start_function.

stop() ->
    ok.
