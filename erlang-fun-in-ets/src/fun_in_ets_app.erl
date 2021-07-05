%%%-------------------------------------------------------------------
%% @doc fun_in_ets public API
%% @end
%%%-------------------------------------------------------------------

-module(fun_in_ets_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    fun_in_ets_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
