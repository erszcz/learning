-module(permutations).
-compile([export_all]).

generate(1, Xs) -> [ [X] || X <- Xs ];
generate(N, Xs) ->
    Ps = generate(N-1, Xs),
    [ [X | P] || X <- Xs, P <- Ps ].

%% How to write a generic next/1 without typeclasses / interfaces?
next(_) ->
    error(not_implemented).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").
-define(eq(E, A), ?assertEqual(E, A)).

generate_test() ->
    ?eq(["0", "1"],
        generate(1, "01")),
    ?eq(["00", "01", "10", "11"],
        generate(2, "01")),
    ?eq(["000", "001", "010", "011",
         "100", "101", "110", "111"],
        generate(3, "01")),
    ?eq(["0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111",
         "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111"],
        generate(4, "01")).

-endif.
