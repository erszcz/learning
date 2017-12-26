%%%-------------------------------------------------------------------
%% @doc cfexample public API
%% @end
%%%-------------------------------------------------------------------

-module(cfexample_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    cfexample_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================