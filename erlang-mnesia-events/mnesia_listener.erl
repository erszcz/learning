-module(mnesia_listener).

-compile([export_all]).

-define(EVENT_TYPE, {table, some_record, simple}).

start() ->
    Pid = spawn(?MODULE, init, []),
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! stop.

init() ->
    mnesia:subscribe(?EVENT_TYPE),
    loop().

loop() ->
    receive
        stop ->
            mnesia:unsubscribe(?EVENT_TYPE),
            ok;
        {mnesia_table_event, {Oper, Record, ActivityId}} ->
            error_logger:info_msg("Mnesia operation ~p on ~p in activity ~p~n~n",
                                  [Oper, Record, ActivityId]),
            loop();
        Msg ->
            error_logger:info_msg("Unexpected message received: ~p~n~n", [Msg]),
            loop()
    end.
