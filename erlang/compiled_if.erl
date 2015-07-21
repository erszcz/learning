-module(compiled_if).

-compile([export_all]).

condition() ->
    1 == random:uniform(2).

f() ->
    Condition = condition(),
    if
        Condition -> ok;
        not Condition -> not_ok
    end.

some_val() ->
    123.

f2() ->
    A = some_val(),
    if
        (3 rem A) == 0 ->
            equals_zero;
        not (3 rem A == 0) ->
            does_not_equal_zero
    end.
