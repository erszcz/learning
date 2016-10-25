-module(tryof).
-compile([export_all]).

error_from_condition() ->
    try error(error_from_condition) of
        _ -> ok
    catch C:E -> {caught, {C,E}} end.

error_from_clause() ->
    try a of
        a -> error(error_from_clause);
        _ -> ok
    catch C:E -> {caught, {C,E}} end.

error_case_clause() ->
    try z of
        a -> error(error_case_clause);
        b -> ok
    catch C:E -> {caught, {C,E}} end.
