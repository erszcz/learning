-module(sups).

-compile([export_all]).

start() ->
    application:start(sasl),
    application:start(sups).

stop() ->
    application:stop(sups),
    application:stop(sasl).
