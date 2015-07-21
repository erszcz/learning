-module(tracefun).

-export([start/0,
         stop/0]).

-include_lib("stdlib/include/ms_transform.hrl").

start(User) ->
    dbg:tracer(process, {fun trace/2, initial_state()}),
    dbg:p(all, call),
    dbg:tp(ejabberd_c2s,
           dbg:fun2ms(fun([_, #state{user = User}]) ->
                    return_trace()
           end)).

initial_state() ->
    D1 = dict:new(),
    dict:store(initialized, os:timestamp(), D1).

stop() ->
    dbg:stop().

trace({trace, Pid, call,
       {ejabberd_c2s, Function, [_Msg, SD]}}, Dict) ->
    io:format("(~p) c ~p/2 jid=~p user=~p~n", [Pid, Function, SD#state.jid, SD#state.user]),
    Dict1 = dict:update(ncalls,
                        fun(NCalls) -> NCalls + 1 end,
                        0, Dict),
    dict:store({calltime, Function}, os:timestamp(), Dict1);
trace({trace, Pid, return_from,
       {ejabberd_c2s, Function, [_Msg, SD]}, _}, Dict) ->
    io:format("(~p) r ~p/2 jid=~p user=~p~n", [Pid, Function, SD#state.jid, SD#state.user]),
    Dict1 = dict:update(nreturns,
                        fun(NReturns) -> NReturns + 1 end,
                        0, Dict),
    {ok, Call} = dict:find({calltime, Function}, Dict1),
    CallDuration = timer:now_diff(os:timestamp(), Call),
    Dict2 = dict:update({total, Function},
                        fun(Total) -> Total + CallDuration end,
                        0, Dict1),
    %erlang:display({Function, CallDuration}),
    file:write_file("dump.dat", format_dict(Dict2)),
    Dict2;
trace(M, Dict) ->
    erlang:display(M),
    Dict.

format_dict(Dict) ->
    {ok, Initialized} = dict:find(initialized, Dict),
    {ok, NCalls} = dict:find(ncalls, Dict),
    {ok, NReturns} = dict:find(nreturns, Dict),
    Totals = [io_lib:format("~p ~p", [Function, erlang:round(Total / 1000)])
              || {{total, Function}, Total} <- dict:to_list(Dict)],
    T = string:join(Totals, "~n"),
    io_lib:format("# ~p~n# ncalls = ~p~n# nreturns = ~p~n" ++ T,
                  [calendar:now_to_local_time(Initialized), NCalls, NReturns]).
