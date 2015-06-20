-module(hello_elli).

-export([start/0,
         stop/0]).

start() ->
    application:start(elli).
    
stop() ->
    application:stop(elli).
