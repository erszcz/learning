-module(test).

-export([store/0,
         f/0]).

store() ->
    %ets:insert(table, {f, fun f/0}).
    ets:insert(table, {f, fun ?MODULE:f/0}).

f() ->
    %before_reload.
    after_reload.
