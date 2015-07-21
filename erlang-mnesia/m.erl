-module(m).

-compile([export_all]).

-record(rec, {name=undefined, age=0}).

create() ->
    mnesia:start(),
    mnesia:create_table(rec,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, rec)}]),
    mnesia:add_table_index(rec, age),
    mnesia:add_table_copy(rec, node(), ram_copies).

fill() ->
    [ mnesia:dirty_write(#rec{name=rand(names()),
                              age=random:uniform(50)})
     || _ <- lists:seq(1, 10) ].

names() ->
    [john, alice, jane, adam, chris, mike].

rand(L) ->
    lists:nth(random:uniform(length(L)), L).

dirty_dirty(Val, Index) ->
    F = fun() ->
            mnesia:dirty_index_read(rec, Val, Index)
        end,
    mnesia:sync_dirty(F).

op_dirty(Val, Index) ->
    F = fun() ->
            mnesia:index_read(rec, Val, Index)
        end,
    mnesia:sync_dirty(F).

op_tran(Val, Index) ->
    F = fun() ->
            mnesia:index_read(rec, Val, Index)
        end,
    mnesia:transaction(F).

dirty_tran(Val, Index) ->
    F = fun() ->
            mnesia:dirty_index_read(rec, Val, Index)
        end,
    mnesia:transaction(F).
