-module(exprecs_example).
%-compile(export_all).
-compile({parse_transform, exprecs}).

-record(r, {a = 0 :: integer(),
            b = 0 :: integer(),
            c = 0 :: integer()}).
-record(s,{a}).

%-export_records([r,s]).
-export_records([r]).

-exprecs_prefix([operation]).
-exprecs_fname([prefix, "_", record]).
-exprecs_vfname([fname, "__", version]).

f() ->
    {new, new_r([])}.
