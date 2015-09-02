%% This header file is intended for code generation inside a .erl file
%% (a file defining an opaque record).
%% Therefore, on inclusion it requires LATEST_RECORD_VERSION macro to be defined.
%% Assume it "exports" the following from a module including it:
%%
%% -export([fields/0,
%%          get/2]).
%%
%% While, apart from LATEST_RECORD_VERSION, it expects convert/2,3 functions
%% to be defined in the module.

fields() ->
    %% This function can't be generalized over different records
    %% as record_info is a compiler hack not a regular Erlang function.
    %% I.e. the record name (myrec_v2 here) has to be a literal below
    %% and _can not be a variable_.
    %% Always make this point at the newest record version!
    [?LATEST_RECORD_VERSION | record_info(fields, ?LATEST_RECORD_VERSION)].

get(Record, Field) -> vr_lib:get(Record, Field, fields()).
