-module(pt_auto_properties).

-export([parse_transform/2]).

-define(a2l(Atom), atom_to_list(Atom)).

parse_transform(AST, _Options) ->
    Forms = erl_syntax:form_list(AST),
    %io:format("forms:~n~p~n", [Forms]),
    Module = get_module(Forms),
    %io:format("~p:~n~p~n", [Module, AST]),
    SuiteType = suite_type(Module),
    Props = get_properties(SuiteType, Forms),
    io:format("~p:~n~p~n", ["props", Props]),
    AST.

get_module(Forms) ->
    catch erl_syntax_lib:fold(fun throw_module/2, undefined, Forms).

throw_module({attribute, _, module, Module}, _) -> throw(Module);
throw_module(_, Acc)                            -> Acc.

suite_type(Module) ->
    LModule = ?a2l(Module),
    case lists:reverse(LModule) of
        %% *_tests
        "stset_" ++ _ -> eunit;
        %% *_SUITE
        "ETIUS_" ++ _ -> ct
    end.

get_properties(SuiteType, Forms) ->
    erl_syntax_lib:fold(mk_get_properties(SuiteType), [], Forms).

mk_get_properties(SuiteType) ->
    Arity = case SuiteType of eunit -> 0; ct -> 1 end,
    fun (Node, Props) ->
        get_properties(Arity, Node, Props)
    end.

get_properties(Arity, Node, Props) ->
    try
        {Name, Arity} = erl_syntax_lib:analyze_function(Node),
        true = is_property(Name),
        [{Name, pos(Node)} | Props]
    catch
        throw:syntax_error  -> Props;
        error:{badmatch, _} -> Props
    end.

is_property(Name) ->
    case ?a2l(Name) of
        "prop_" ++ _ -> true;
        _ -> false
    end.

pos(Node) ->
    erl_syntax:abstract(erl_syntax:get_pos(Node)).
