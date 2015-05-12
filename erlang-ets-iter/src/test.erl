-module(test).

-compile([export_all]).

%% Show difference between folding over a table using ets:first and ets:next
%% without ets:safe_fixtable and with it.
%% Without fixing the table it's not possible to delete elements safely,
%% with fixing all modifications are allowed.

iter() -> fun (E, Acc) -> [E | Acc] end.

iter_delete() -> fun ({Key}, Acc) -> ets:delete(t, Key), [{Key} | Acc] end.

test(F, Fold) ->
    try
        t = ets:new(t, [named_table, public]),
        random:seed(),
        N = 5,
        [ ets:insert(t, {random:uniform(100)}) || _ <- lists:seq(1, N) ],
        Fold(F, [], t)
    after
        ets:delete(t)
    end.

lame_fold(F, Acc0, T) ->
    lame_fold(F, Acc0, ets:first(T), T).

lame_fold(_F, Acc, '$end_of_table', _T) -> Acc;
lame_fold(F, Acc, Key, T) ->
    lame_fold(F,
              lists:foldl(F, Acc, ets:lookup(T, Key)),
              ets:next(T, Key),
              T).

fold(F, Acc0, T) ->
    ets:safe_fixtable(T, true),
    fold(F, Acc0, ets:first(T), T).

fold(_F, Acc, '$end_of_table', T) ->
    ets:safe_fixtable(T, false),
    Acc;
fold(F, Acc, Key, T) ->
    fold(F,
         lists:foldl(F, Acc, ets:lookup(T, Key)),
         ets:next(T, Key),
         T).
