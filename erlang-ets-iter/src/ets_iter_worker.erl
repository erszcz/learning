-module(ets_iter_worker).

-behaviour(gen_server).

%% API
-export([
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?MODULE, [public, named_table]),
    random:seed(erlang:now()),
    self() ! {seed, 5},
    {ok, #state{}}.

handle_call(test, _From, State) ->
    Reply = {ok, iter(?MODULE)},
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({seed, N}, State) ->
    [ ets:insert(?MODULE, {random:uniform(100)}) || _ <- lists:seq(1, N) ],
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

iter(Table) ->
    ets:foldl(fun step/2, {true, []}, Table).

step({I} = Object, {Flag, Acc}) ->
    if
        Flag -> ets:delete_object(?MODULE, Object);
        not Flag -> ok
    end,
    {not Flag, [Object | Acc]}.
