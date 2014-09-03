-module(mnesia_helper).

-compile([export_all]).

-record(some_record, {name=default, value=undefined}).

setup() ->
    application:start(sasl),
    application:start(mnesia),
    {atomic, ok} = mnesia:create_table(some_record, [{attributes,
                                                      record_info(fields, some_record)}]).
