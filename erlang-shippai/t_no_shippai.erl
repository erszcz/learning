-module(t_no_shippai).
-export([t/1]).

t(X) ->
    case X of
        {Y,Z} -> Y + Z
    end.
