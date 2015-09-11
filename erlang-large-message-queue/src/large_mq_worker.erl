-module(large_mq_worker).
-compile([export_all]).

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

-record(state, {blobs}).

grow_state(BinaryOrTuple) ->
    io:format("~ngrow state: ~p~n", [BinaryOrTuple]),
    ?SERVER ! {grow_state, BinaryOrTuple}.

grow_mq() ->
    io:format("~ngrow mq~n", []),
    ?SERVER ! grow_mq.

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{blobs = []}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({grow_state, B}, #state{blobs = Bs} = S) ->
    io:format("."),
    NS = S#state{blobs = [B | Bs]},
    erlang:send_after(timer:seconds(1), self(), {grow_state, blob(B)}),
    {noreply, NS};
handle_info(grow_mq, #state{} = S) ->
    spawn_link(?MODULE, messenger_loop, [self()]),
    timer:sleep(timer:hours(3)),
    {noreply, S};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

blob(B) when is_binary(B) -> big_binary();
blob(B) when is_tuple(B)  -> big_tuple().

big_binary() ->
    Rand = crypto:rand_bytes(256),
    << <<Rand/bytes>> || _ <- lists:seq(1, 128) >>.

big_tuple() ->
    S = 256,
    Integers = binary_to_list(crypto:rand_bytes(S)),
    erlang:make_tuple(S, lists:zip(lists:seq(1, S), Integers)).

messenger_loop(Pid) ->
    messenger_loop(Pid, 0).

messenger_loop(Pid, N) ->
    Pid ! {msg, N},
    timer:sleep(100),
    messenger_loop(Pid, N+1).
