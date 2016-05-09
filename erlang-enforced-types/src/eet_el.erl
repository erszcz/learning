-module(eet_el).
-compile([export_all]).

-export_type([t/0]).

-record(xmlel, {name,
                attrs = [],
                children = []}).

-opaque t() :: #xmlel{name :: binary(),
                      attrs :: list(),
                      children :: [?MODULE:t() | eet_cdata:t()]}.

-spec new(Name, Attrs, Children) -> t() when
      Name :: binary(),
      Attrs :: [],
      Children :: [eet_cdata:t() | t()].
new(Name, Attrs, Children) ->
    #xmlel{name = Name,
           attrs = Attrs,
           children = Children}.
