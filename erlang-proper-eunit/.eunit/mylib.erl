-module(mylib).

%% mylib: mylib library's entry point.

-export([my_func/0]).


%% API

my_func() ->
    ok().

%% Internals

ok() ->
    ok.

%% End of Module.
