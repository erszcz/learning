-module(elixir_libs).

-export([test/0]).

-define(Date, 'Elixir.Timex.Date').
-define(DateF, 'Elixir.Timex.DateFormat').

test() ->
    Now = ?Date:from(calendar:local_time(), <<"Europe/Warsaw">>),
    print("~p~n", [Now]),
    print("~p~n", [?Date:universal()]),
    print("~p~n", [?Date:local()]),
    print("ISO : ~s~n", [element(2, ?DateF:format(Now, <<"{ISO}">>))]),
    print("ISOz: ~s~n", [element(2, ?DateF:format(Now, <<"{ISOz}">>))]).

print(Text) ->
    io:format(Text, []).

print(Fmt, Args) ->
    io:format(Fmt, Args).
