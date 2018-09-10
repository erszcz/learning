-module(cf_consumer).
-behaviour(gen_server).

%% API
-export([
         start_link/0,
         consume/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(CONSUMER_CREDIT, {4, 2}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

consume(N) ->
    error_logger:info_msg("consume: ~p pdict: ~p\n", [N, erlang:get()]),
    credit_flow:send({local, ?SERVER}, ?CONSUMER_CREDIT),
    gen_server:cast({local, ?SERVER}, {consume, self(), N}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({consume, From, N}, State) ->
    handle_consume(From, N),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_consume(From, N) ->
    error_logger:info_msg("handle_consume: ~p pdict: ~p\n", [N, erlang:get()]),
    timer:sleep(consumption_time()),
    credit_flow:ack(From, ?CONSUMER_CREDIT),
    error_logger:info_msg("handle_consume - acked: ~p pdict: ~p\n", [N, erlang:get()]),
    ok.

consumption_time() ->
    timer:seconds(2).
