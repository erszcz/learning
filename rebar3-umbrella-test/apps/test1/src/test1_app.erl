%%%-------------------------------------------------------------------
%% @doc test1 public API
%% @end
%%%-------------------------------------------------------------------

-module(test1_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    test1_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
