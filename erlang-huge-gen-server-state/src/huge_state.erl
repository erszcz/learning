-module(huge_state).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-compile({no_auto_import, [size/1, tuple_size/1]}).

-define(SERVER, ?MODULE).

-define(timestamp(), erlang:system_time(millisecond)).
-define(giga_byte, 1073741824).
%-define(initial_baggage, crypto:strong_rand_bytes(128)).
-define(initial_baggage, {$a, $a}).

-record(state, {action,
                prev_ts,
                ts,
                baggage,
                reps = 0}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    self() ! step,
    {ok, #state{action = grow,
                ts = ?timestamp(),
                baggage = ?initial_baggage}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(step, #state{reps = Reps} = St) when Reps >= 20 ->
    io:format("State is not growing anymore and repetition limit hit\n", []),
    {noreply, St};
handle_info(step, St) ->
    #state{action = Action,
           ts = TS,
           baggage = Baggage,
           reps = Reps} = St,
    {NewAction, NewReps} = case size(Baggage) < 1 * ?giga_byte of
                               true -> {grow, Reps};
                               false -> {pass, Reps + 1}
                           end,
    NewSt = St#state{action = NewAction,
                     prev_ts = TS,
                     ts = ?timestamp(),
                     baggage = case Action of
                                   grow -> double(Baggage);
                                   pass -> Baggage
                               end,
                     reps = NewReps},
    print_stats(NewSt),
    self() ! step,
    {noreply, NewSt};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

size(B) when is_binary(B) -> erlang:byte_size(B);
size(T) when is_tuple(T) -> tuple_size(T).

tuple_size(I) when is_integer(I) -> 1;
tuple_size({T1, T2}) -> 2 + tuple_size(T1) + tuple_size(T2).

double(B) when is_binary(B) ->
    <<B/bytes, B/bytes>>;
double(T) when is_tuple(T) ->
    {T, T}.

print_stats(St) ->
    #state{prev_ts = LastTs,
           ts = TS,
           baggage = Baggage} = St,
    io:format("""
              Elapsed (milliseconds): ~p
              Baggage size (megabytes): ~p~n~n
              """,
              [(TS - LastTs), size(Baggage) / 1024 / 1024]).
