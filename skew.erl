%%% File    : skew.erl
%%% Author  : Pierpaolo BERNARDI <olopierpa@gmail.com>
%%% Description : Skew heaps
%%% Created : 30 May 2003 by Pierpaolo BERNARDI <olopierpa@gmail.com>

-module(skew).

-export([empty/0,
         is_empty/1,
         min/1,
         delete_min/1,
         insert/2,
         merge/2]).


%% Aggressive inlining - will increase code size.
%%-compile(inline).
%%-compile({inline_size,100}).

%%-define(TESTING,true).

-ifdef(TESTING).
-compile(export_all).
-endif.

-define(THE_EMPTY_HEAP, the_empty_heap).

empty() ->
    ?THE_EMPTY_HEAP.

is_empty(?THE_EMPTY_HEAP) -> true;
is_empty(_) -> false.

min({X, _, _}) -> X.

delete_min({_X, A, B}) -> merge(A, B).

insert(X, A) ->
    merge({X, ?THE_EMPTY_HEAP, ?THE_EMPTY_HEAP}, A).

merge(A, ?THE_EMPTY_HEAP) ->
    A;
merge(?THE_EMPTY_HEAP, B) ->
    B;
merge({MA, LA, RA}, B={MB, _, _}) when MA =< MB ->
    {MA, RA, merge(LA, B)};
merge(A, {M, L, R}) ->
    {M, R, merge(L, A)}.


-ifdef(TESTING).

test() ->
    test(100000, 1, 1000000000).

test(N, Da, A) ->
    pblib:randomize(),
    Q = fa_random_heap(N, Da, A),
    scrivi_heap(Q).

misura() ->
    T0 = pblib:mo(),
    _Q = fa_random_heap(100000, 1, 1000000000),
    T1 = pblib:mo(),
    T1 - T0.

fa_random_heap(Quanti, Da, A) ->
    fa_random_heap(Quanti, Da, A, empty()).

fa_random_heap(0, _Da, _A, Q) ->
    Q;
fa_random_heap(N, Da, A, Q) ->
    fa_random_heap(N-1, Da, A, insert(random:uniform(A- Da+1)+Da-1, Q)).


fa_random_heap2(Quanti, Da, A) ->
    fa_random_heap2(Quanti, Da, A, empty()).

fa_random_heap2(0, _Da, _A, Q) ->
    Q;
fa_random_heap2(N, Da, A, Q) ->
    fa_random_heap2(N-1, Da, A, insert(1000000-N, Q)).

scrivi_heap(Q) ->
    case is_empty(Q) of
	true ->
	    ok;
	false ->
	    M = min(Q),
	    Q1 = delete_min(Q),
	    io:fwrite("~s\n", [pblib:ultospun(M)]),
	    scrivi_heap(Q1)
    end.

profo(?THE_EMPTY_HEAP) ->
    0;
profo({_, A, B}) ->
    1 + lists:max([profo(A),
		   profo(B)]).

dimen(?THE_EMPTY_HEAP) ->
    0;
dimen({_, A, B}) ->
    1 + dimen(A) + dimen(B).

-endif.
