-module(mylib).

%% mylib: mylib library's entry point.

-export([my_func/1]).


%% API

my_func(A) ->
    ok(A).

%% Internals

ok(_) ->
    ok.

%% End of Module.
