-module(folsom_99p).

%% folsom_99p: folsom_99p library's entry point.

-export([generate_stats/0]).

-compile([export_all]).

%% API

generate_stats() ->
    [].

%% Internals

randomized_hit(_, {St, Acc}) ->
    N = (St * 16#1337ac3) rem (1 bsl 32),
    folsom_metrics:notify(rand_val, N, histogram),
    {N, [N | Acc]}.

%histogram_timed(Name, Thunk) ->
%    {Time, Result} = timer:tc(Thunk),
%    Diff = Time div 1000,
%    _ = folsom_metrics:notify(Name, Diff, histogram),
%    Result.

%% End of Module.
