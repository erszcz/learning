-module(mhs).
-compile([export_all, nowarn_export_all]).

init() ->
    %% The default min_heap_size is 233.
    %% max_heap_size has to be bigger than that.
    erlang:process_flag(max_heap_size, 666),
    Init = <<"ala-ma-kota-">>,
    loop(Init).

loop(Data) ->
    error_logger:info_msg("data len: ~p\n", [byte_size(Data)]),
    timer:sleep(1000),
    loop(<<Data/bytes, Data/bytes>>).
