-module(my_rec).

-compile([export_all]).

-behaviour(fmt).

-record(my_rec, {a,b}).

new() -> #my_rec{}.

to_str() -> "my_rec()".
