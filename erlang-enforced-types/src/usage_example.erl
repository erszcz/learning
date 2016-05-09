-module(usage_example).
-compile([export_all]).

-spec invalid() -> eet_el:t().
invalid() ->
    eet_el:new(<<"invalid">>,
               [],
               [<<"should-cause-error">>]).
    %#xmlel{name = <<"invalid">>,
    %       children = [<<"should-cause-error">>]}.

-spec valid() -> eet_el:t().
valid() ->
    eet_el:new(<<"invalid">>,
               [],
               [eet_cdata:from_binary(<<"should-be-ok">>)]).
    %#xmlel{name = <<"valid">>,
    %       children = [eet_cdata:from_binary(<<"should-be-ok">>)]}.
