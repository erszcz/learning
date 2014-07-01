-module(test_SUITE).

-compile([export_all]).

-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

prop_1() ->
    ?FORALL(X, integer(0, 10), X =:= X).
