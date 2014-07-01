-module(test2_SUITE).

-compile([export_all]).

-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

-proper_opts([{eunit_env, foo/1}]).
foo(Test) ->
    {setup, fun() -> zzz end, Test}.

prop_1() ->
    ?FORALL(X, integer(0, 10), X =:= X).
