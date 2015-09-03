-module(myrec).
-compile([export_all]).

-include("myrec.hrl").

%% Use after your record versions are defined!
-define(LATEST_RECORD_VERSION, myrec_v2).
-include("versioned_record.hrl").
-undef(LATEST_RECORD_VERSION).

%% This might require application-specific logic - override as applicable.
upgrade(#myrec_v1{} = V1) ->
    vr_lib:upgrade(V1, #myrec_v2{},
                   %% These are the fields new in v2 along with default values.
                   [{#myrec_v2.field2, (#myrec_v2{})#myrec_v2.field2}]).
