-module(type_encoded_properties).
-compile([export_all]).

-define(N_COMMON_FIELDS, 2).
-define(COMMON_FIELDS, field1=val1,
                       field2=val2).

-record(unauth_state, {?COMMON_FIELDS,
                       extra_unauth_field3=val3}).

-record(auth_state, {?COMMON_FIELDS,
                     extra_auth_field3=otherval3}).

-spec authenticate(UnauthState) -> AuthState when
      UnauthState :: #unauth_state{},
      AuthState :: #auth_state{}.
authenticate(#unauth_state{} = US) ->
    %% do clever stuff and then:
    #auth_state{field1 = US#unauth_state.field1,
                field2 = US#unauth_state.field2}.

state_before_auth(#unauth_state{} = US) ->
    %% some action, probably proceed to auth
    authenticate(US).

%% `function_clause` for unauthenticated user
state_after_auth(#auth_state{} = AS) ->
    %% allow requests requiring authentication
    do_things(AS).
%% possibly catch other cases and log violation
%state_after_auth(S) ->
%    log_violation(S).

do_things(_) -> ok.
