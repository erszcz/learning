-module(eet_cdata).
-compile([export_all]).

%% Importable
-export([binary_to_cdata/1,
         cdata_to_binary/1]).

-export_type([t/0]).

-record(xmlcdata, {content = []}).

-opaque t() :: #xmlcdata{content :: iodata()}.

%%
%% Importable
%%

binary_to_cdata(B) ->
    from_binary(B).

cdata_to_binary(CData) ->
    to_binary(CData).

%%
%% Qualified interface (i.e. mod:fun(Args) calling style)
%%

-spec from_binary(binary()) -> ?MODULE:t().
from_binary(B) ->
    exml:escape_cdata(B).

-spec to_binary(t()) -> binary().
to_binary(#xmlcdata{content = CData}) ->
    exml:unescape_cdata(CData).
