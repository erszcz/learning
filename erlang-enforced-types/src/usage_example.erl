-module(usage_example).
-compile([export_all]).

-record(xmlel, {name,
                attrs = [],
                children = []}).

-type xmlel() :: #xmlel{name :: binary(),
                        attrs :: list(),
                        children :: [xmlel() | eet_cdata:t()]}.

-spec invalid() -> xmlel().
invalid() ->
    xmlel(<<"invalid">>,
          [],
          [<<"should-cause-error">>]).
    %#xmlel{name = <<"invalid">>,
    %       children = [<<"should-cause-error">>]}.

-spec valid() -> xmlel().
valid() ->
    xmlel(<<"invalid">>,
          [],
          [eet_cdata:from_binary(<<"should-be-ok">>)]).
    %#xmlel{name = <<"valid">>,
    %       children = [eet_cdata:from_binary(<<"should-be-ok">>)]}.

-spec xmlel(Name, Attrs, Children) -> xmlel() when
      Name :: binary(),
      Attrs :: [],
      Children :: [eet_cdata:t() | xmlel()].
xmlel(Name, Attrs, Children) ->
    #xmlel{name = Name,
           attrs = Attrs,
           children = Children}.
