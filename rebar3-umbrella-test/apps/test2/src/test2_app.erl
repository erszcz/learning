%%%-------------------------------------------------------------------
%% @doc test2 public API
%% @end
%%%-------------------------------------------------------------------

-module(test2_app).

-behaviour(application).

-export([start/2, stop/1, test/0]).

%-include("apps/test1/include/test1.hrl").
-include_lib("test1/include/test1.hrl").

start(_StartType, _StartArgs) ->
    test2_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

test() ->
    ?TEST1_MACRO.
