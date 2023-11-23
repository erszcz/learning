-module(nat_encoding).
-export([zero/0,
         succ/1,
         is_zero/1,
         to_int/1,
         from_int/1,
         add/2,
         mul/2]).
-export([two/0,
         three/0]).

% Define atoms for zero and succ
zero() -> zero.
succ(zero) -> {succ, zero};
succ(N) when is_tuple(N), element(1, N) == succ -> {succ, N}.

% Check if a tuple represents zero
is_zero(zero) -> true;
is_zero(_) -> false.

% Convert a tagged tuple to an integer
to_int(zero) -> 0;
to_int({succ, N}) -> 1 + to_int(N).

% Convert an integer to its tuple-encoded representation
from_int(0) -> zero;
from_int(N) when N > 0 -> succ(from_int(N - 1)).

% Examples
two() -> succ(succ(zero)).
three() -> succ(succ(succ(zero))).

% Addition of natural numbers
add(N, zero) -> N;
add(N, {succ, M}) -> succ(add(N, M)).

% Multiplication of natural numbers
mul(_, zero) -> zero;
mul(N, {succ, M}) -> add(N, mul(N, M)).
