-module(ct_rsh_SUITE).
-compile([export_all]).

all() -> [test].

test(_) ->
    ct:break("should break now"),
    ct:pal("resumed"),
    ok.
