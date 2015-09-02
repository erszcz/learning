-module(vr_lib).
-compile([export_all]).

get(Record, Field, Fields) ->
    case index(Field, Fields) of
        {error, missing_field} -> undefined;
        Index when is_integer(Index), Index > size(Record) -> undefined;
        Index when is_integer(Index) -> element(Index, Record)
    end.

%% This is just a mapping from field name to field number for more convenient use
%% of erlang:element/2 on records-as-tuples.
index(Field, Fields) when is_atom(Field) ->
    case get_field_index(Field, Fields) of
        false -> {error, missing_field};
        {Field, Index} -> Index
    end.

get_field_index(Field, Fields) ->
    lists:keyfind(Field, 1, lists:zip(Fields, lists:seq(1, length(Fields)))).
