-module(lager_tracing).

-compile([export_all]).

-include_lib("lager/include/lager.hrl").

log_event_with_tag() ->
    lager:warning([{tag, true}], "warning with tag ~p", ["{tag, true}"]).

log_event_no_tag() ->
    lager:warning("warning without tags", []).
