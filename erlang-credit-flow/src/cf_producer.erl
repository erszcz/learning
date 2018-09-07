-module(cf_producer).
-behaviour(gen_server).

%% API
-export([
         start_link/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link(N) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [N], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([N]) ->
    {ok, #{remaining => N}, 0}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #{remaining := N} = State) ->
    error_logger:info_msg("timeout received at N=~p\n", [N]),
    produce(N),
    {noreply, State#{remaining := N-1}, produce_interval()};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

produce_interval() ->
    timer:seconds(1).

produce(N) ->
    error_logger:info_msg("produce: ~p pdict: ~p\n", [N, erlang:get()]),
    wait_for_credit(),
    cf_consumer:consume(N).

wait_for_credit() ->
    case credit_flow:blocked() of
        false ->
            error_logger:info_msg("producer not blocked, pdict: ~p\n",
                                  [erlang:get()]),
            ok;
        true  ->
            error_logger:info_msg("producer blocked - waiting for credit, pdict: ~p\n",
                                  [erlang:get()]),
            receive
                {bump_credit, Msg} ->
                    error_logger:info_msg("received bump_credit, pdict: ~p\n", [erlang:get()]),
                    credit_flow:handle_bump_msg(Msg),
                    error_logger:info_msg("credit bumped, pdict: ~p\n", [erlang:get()])
                after bump_timeout() ->
                    error(bump_timeout)
            end
    end.

bump_timeout() ->
    timer:seconds(60).
