-module(join).

-export([binaries/2,
         list/2]).

-spec binaries(Binaries, Separator) -> binary()
    when Binaries :: [binary()], Separator :: binary().
binaries([], _Separator) ->
    <<>>;
binaries([B], _Separator) ->
    B;
binaries([H | T], Separator) ->
    << H/binary, << <<Separator/binary, B/binary>> || B <- T >>/binary >>.

-spec list([any()], Separator) -> list() when Separator :: any().
list([H | T], Sep) ->
    F = fun(E, Acc) ->
            [E, Sep] ++ Acc
    end,
    [H | lists:reverse(lists:foldl(F, [], T))].
