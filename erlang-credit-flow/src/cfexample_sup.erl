%%%-------------------------------------------------------------------
%% @doc cfexample top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cfexample_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { #{strategy => one_for_one},
           [consumer(),
            producer()] }}.

%%====================================================================
%% Internal functions
%%====================================================================

consumer() ->
    #{id => cf_consumer,
      start => {cf_consumer, start_link, []}}.

producer() ->
    #{id => cf_producer,
      start => {cf_producer, start_link, [20]}}.
