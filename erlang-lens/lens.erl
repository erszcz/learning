-module(lens).

-compile([export_all]).

-record(address, {street, city, state, zip}).
-record(person, {name, age, addr}).

-define(RecLens(RecName, Fname),
        Fname({Vmod, Fmap}) -> {fun(R = #RecName{Fname=F}) ->
                                        Fmap(fun(NewF) -> R#RecName{Fname=NewF} end, Vmod(F))
                                end, Fmap}).

?RecLens(address, street).
?RecLens(address, city).
?RecLens(address, state).
?RecLens(address, zip).

?RecLens(person, name).
?RecLens(person, age).
?RecLens(person, addr).

runlens(Lens, Obj, FMod) ->
    {L, _} = Lens(FMod),
    L(Obj).

set(Lens, Obj, Val) ->
    runlens(Lens, Obj, {fun(_) -> Val end, fun(F,V) -> F(V) end}).

view(Lens, Obj) ->
    runlens(Lens, Obj, {fun(V) -> V end, fun(_F, V) -> V end}).

modify(Lens, Obj, Func) ->
    runlens(Lens, Obj, {fun(V) -> Func(V) end, fun(F,V) -> F(V) end}).

compose(L1, L2) -> fun(O) -> L1(L2(O)) end.

t() ->
    A = #address{street="123 Main St", city="Oakland", state="CA", zip="94610"},
    P = #person{name="Steven", age=46, addr=A},
    % Use a lens as an accessor
    io:format("Name: ~p\n", [view(fun name/1, P)]),
    % Use the same lens as a setter
    P1 = set(fun name/1, P, "NewDude"),
    io:format("New Person: ~p\n", [P1]),
    % Create a lens by composing two other lenses
    AddrStreet = compose(fun addr/1, fun street/1),
    % Use it to set a nested field
    P2 = set(AddrStreet, P, "456 New Rd."),
    % Use the same lens to view the nested field
    io:format("New Street: ~p\n", [view(AddrStreet, P2)]),
    % View the entire nested record
    io:format("New Addr~p\n", [view(fun addr/1, P2)]),
    % View the entire top-level record
    io:format("New Person~p\n", [P2]),
    % Modify a field in place
    P3 = modify(fun age/1, P, fun(N) -> N+1 end),
    io:format("New Age: ~p\n", [view(fun age/1, P3)]). 
