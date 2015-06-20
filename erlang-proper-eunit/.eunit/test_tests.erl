-module(test_tests).

-include_lib("proper_eunit/include/pt_proper_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").

test_test() ->
    ?assert(test:my_func() == ok).

prop_1() ->
    ?FORALL(X, integer(0, 10), X =:= X).
