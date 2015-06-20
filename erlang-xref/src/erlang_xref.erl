-module(erlang_xref).

-export([my_func/0]).


%% API

my_func() ->
    ok().

%% Internals

ok() ->
    [external_module:a(), external_module:b()].

%% End of Module.
