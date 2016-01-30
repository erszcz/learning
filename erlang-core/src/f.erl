-module(ex1).

%-compile(inline).
%-compile({inline_size,0}).

-export([f/1]).

f(X) ->
    case X of
        {foo, A} -> B = g(A);
        {bar, A} -> B = h(A)
    end,
    {A, B}.

g(A) -> {ok1, A}.

h(A) -> {ok2, A}.
