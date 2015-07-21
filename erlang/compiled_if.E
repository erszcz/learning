-file("compiled_if.erl", 1).

condition() ->
    1 == random:uniform(2).

f() ->
    Condition = condition(),
    if
        Condition ->
            ok;
        not Condition ->
            not_ok
    end.

module_info() ->
    erlang:get_module_info(compiled_if).

module_info(X) ->
    erlang:get_module_info(compiled_if, X).

