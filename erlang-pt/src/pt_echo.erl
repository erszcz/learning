-module(pt_echo).
-export([parse_transform/2]).

parse_transform(AST, _Options) ->
    io:format("~p~n", [AST]),
    AST.
