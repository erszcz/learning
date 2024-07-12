-module(huge_state).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-compile({no_auto_import, [size/1, tuple_size/1]}).

-define(SERVER, ?MODULE).

-define(timestamp(), erlang:system_time(millisecond)).
-define(giga_byte, 1073741824).

-record(state, {action,
                prev_ts,
                ts,
                baggage,
                baggage_size,
                reps = 0}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(BaggageType) when BaggageType =:= binary;
                             BaggageType =:= tuple ->
    Baggage = case BaggageType of
                  binary -> crypto:strong_rand_bytes(128);
                  tuple -> erlang:list_to_tuple(lists:duplicate(100, {}))
              end,
    gen_server:start_link(?MODULE, [Baggage], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Baggage]) ->
    self() ! step,
    {ok, #state{action = grow,
                ts = ?timestamp(),
                baggage = Baggage,
                baggage_size = size(Baggage)}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(step, #state{action = pass, reps = Reps} = St) when Reps >= 20 ->
    io:format("State is not growing anymore and repetition limit hit\n", []),
    {stop, St};
handle_info(step, St) ->
    #state{action = Action,
           ts = TS,
           baggage = Baggage,
           baggage_size = BaggageSize,
           reps = Reps} = St,
    {NewAction, NewReps, NewBaggageSize} =
        case Action of
            grow ->
                {case BaggageSize < 1 * ?giga_byte of
                     true -> grow;
                     false -> pass
                 end, Reps, 2 * BaggageSize};
            pass ->
                {pass, Reps + 1, BaggageSize}
        end,
    NewSt = St#state{action = NewAction,
                     prev_ts = TS,
                     ts = ?timestamp(),
                     baggage = case Action of
                                   grow -> double(Baggage);
                                   pass -> Baggage
                               end,
                     baggage_size = NewBaggageSize,
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

tuple_size(I) when is_integer(I) -> 8;
tuple_size(T) when is_tuple(T) ->
    16 + lists:sum([ tuple_size(E) || E <- erlang:tuple_to_list(T) ]).

double(B) when is_binary(B) ->
    <<B/bytes, B/bytes>>;
double(T) when is_tuple(T) ->
    {T, T}.

print_stats(St) ->
    #state{prev_ts = LastTs,
           ts = TS,
           baggage_size = BaggageSize} = St,
    io:format("""
              Elapsed (milliseconds): ~p
              Baggage size (megabytes): ~p~n~n
              """,
              [(TS - LastTs), BaggageSize / 1024 / 1024]).
