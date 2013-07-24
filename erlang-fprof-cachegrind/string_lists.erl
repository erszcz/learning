-module(string_lists).

-compile([export_all]).

standard_join(StringList, Separator) ->
    string:join(StringList, Separator).

standard_join_test() ->
    List = ["hello", "world", "jim", "asdfsadf", "asdlfjdsakf", "aietree", "ewirh3f3t"],
    standard_join(List, "\n").

join([], _Sep, Acc) ->
    Acc;
join([Head | []], _Sep, Acc) ->
    [Head | Acc];
join([Head | Tail], Sep, Acc) ->
    join(Tail, Sep, [Sep, Head | Acc]).
join(List, Sep) ->
    lists:flatten(lists:reverse(join(List, Sep, []))).

accumulator_join_test() ->
    List = ["hello", "world", "jim", "asdfsadf", "asdlfjdsakf", "aietree", "ewirh3f3t"],
    join(List, "\n").
