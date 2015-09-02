-module(myrec).
-compile([export_all]).

-record(myrec_v1, {field1 = a}).
-record(myrec_v2, {field1 = a,
                   field2 = b}).

-define(LATEST_RECORD_VERSION, myrec_v2).
-include("versioned_record.hrl").
-undef(LATEST_RECORD_VERSION).
