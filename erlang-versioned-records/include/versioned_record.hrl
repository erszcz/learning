%% Description: Generate support code for a versioned opaque record type.
%% Assumptions:
%%
%%  -   LATEST_RECORD_VERSION macro is defined before this file is included;
%%      that is, use this file like this:
%%
%%         -define(LATEST_RECORD_VERSION, myrec_v2).
%%         -include("versioned_record.hrl").
%%         -undef(LATEST_RECORD_VERSION).
%%
%%  -   the module defines an upgrade/1 function handling conversion
%%      of any previous version to the newest record version
%%
%%  -   each new version of the record is a superset of the old versions;
%%      moreover, the order of fields in a new version is the same,
%%      new fields are appended at the end; for example:
%%
%%         -record(myrec_v1, {field1 = a}).
%%         -record(myrec_v2, {field1 = a,
%%                            field2 = b}).
%%
%%      are valid, while:
%%
%%         -record(invalid_rec_v1, {field1 = a}).
%%         -record(invalid_rec_v2, {field0 = field_not_present_in_v1,
%%                                  field1 = a,
%%                                  field2 = b}).
%%      
%%      breaks the contract of this headaer file / library
%%
%%  -   this header file doesn't export anything from the module that imports it;
%%      define your exports explicitly as appropriate;
%%      a suggested -export() attribute is:
%%
%%          -export([fields/0,
%%                   get/2,
%%                   set/3]).
%%
%%      you don't have to export the functions this header generates
%%      and instead can only provide custom accessors as you see fit
%%
%% Limitations:
%%
%%  -   as of now it's not possible to downgrade a record (see vr_lib:set/5
%%      implementation for the reason)

%% Return names of the record fields at runtime.
%% This is an accessible at runtime equivalent of record_info(fields, RecordName).
fields() ->
    %% This function can't be generalized over different records
    %% as record_info is a compiler hack not a regular Erlang function.
    %% I.e. the record name (defined by ?LATEST_RECORD_VERSION) has
    %% to be a literal below and _can not be a variable_.
    %% Make sure ?LATEST_RECORD_VERSION is equal to the newest record version!
    [?LATEST_RECORD_VERSION | record_info(fields, ?LATEST_RECORD_VERSION)].

get(Record, Field) ->
    vr_lib:get(Record, Field, fields()).

set(Record, Field, Value) ->
    vr_lib:set(Record, Field, Value, fields(), fun upgrade/1).
