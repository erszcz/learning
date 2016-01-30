-module(list).
-export([length/1,
         reverse/1]).

-compile({no_auto_import,[length/1]}).

length([_ | Xs]) -> 1 + length(Xs);
length([]) -> 0.

reverse(Xs) ->
    reverse(Xs, []).

reverse([X | Xs], Ys) ->
    reverse(Xs, [X | Ys]);
reverse([], Ys) -> Ys.
