-module(mnesia_test).

-compile([export_all]).

-record(t, {f, g}).

iter() -> fun (E, Acc) -> [E | Acc] end.

iter_delete() ->
    fun (#t{f = Key} = E, {Flag, Acc}) ->
            if
                Flag ->
                    mnesia:dirty_delete(t, Key);
                not Flag ->
                    ok
            end,
            {not Flag, [E | Acc]}
    end.

init() ->
    {atomic, ok} = mnesia:create_table(t,
                                       [{ram_copies, [node()]}, 
                                        {attributes, record_info(fields, t)}]).

test(F, Fold) ->
    try
        random:seed(),
        N = 5,
        [ mnesia:dirty_write(#t{f = random:uniform(100)}) || _ <- lists:seq(1, N) ],
        io:format("~p~n", [ets:tab2list(t)]),
        Fold(F, {true, []}, t),
        io:format("~p~n", [ets:tab2list(t)])
    after
        mnesia:activity(transaction, fun mnesia:delete_table/1, [t])
    end.
