-module(lists3).

-compile([export_all]).

%% API
-export([group_by_w_dict/2,
         group_by_w_map/2]).

%% Tests


%%
%% API
%%

group_by_w_dict(F, L) ->
    dict:to_list( lists:foldr(fun ({K,V}, Acc) -> dict:append(K, V, Acc) end,
                              dict:new(), [ {F(X), X} || X <- L ]) ).

group_by_w_map(F, L) ->
    maps:to_list( lists:foldr(fun ({K,V}, Acc) -> maps:update_with(K, fun (Group) -> [V | Group] end, [V], Acc) end,
                              maps:new(), [ {F(X), X} || X <- L ]) ).

%%
%% Internal
%%

%%
%% Tests
%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

group_by_w_dict_test() ->
    test(fun group_by_w_dict/2).

group_by_w_map_test() ->
    test(fun group_by_w_map/2).

test(F) ->
    #{ in := In, out := Out, key := Key } = data1(),
    ?assertEqual(lists:sort(Out),
                 lists:sort([ {Key, lists:sort(Group)} || {Key, Group} <- F(Key, In) ])).

data1() ->
    #{ out => [{atom, [a, b, c]}, {int, [1, 2, 3]}],
       in  => [c, 2, a, 3, 1, b],
       key => fun (I) when is_integer(I) -> int;
                  (A) when is_atom(A) -> atom end }.

-endif. %% TEST
