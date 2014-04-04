-module(dog).

-behaviour(animal).

-export([says/0,
         does/1]).

%% This is not available in Erlang R16+.
%-extend(animal).

%% These should actually be in a header file for easy reuse.
-define(FORWARD0(M, F), F() -> M:F()).
-define(FORWARD1(M, F), F(A1) -> M:F(A1)).
-define(FORWARD2(M, F), F(A1, A2) -> M:F(A1, A2)).
-define(FORWARD3(M, F), F(A1, A2, A3) -> M:F(A1, A2, A3)).
-define(FORWARD4(M, F), F(A1, A2, A3, A4) -> M:F(A1, A2, A3, A4)).
-define(FORWARD5(M, F), F(A1, A2, A3, A4, A5) -> M:F(A1, A2, A3, A4, A5)).
-define(FORWARD6(M, F), F(A1, A2, A3, A4, A5, A6) -> M:F(A1, A2, A3, A4, A5, A6)).
-define(FORWARD7(M, F), F(A1, A2, A3, A4, A5, A6, A7) -> M:F(A1, A2, A3, A4, A5, A6, A7)).
-define(FORWARD8(M, F), F(A1, A2, A3, A4, A5, A6, A7, A8) -> M:F(A1, A2, A3, A4, A5, A6, A7, A8)).
-define(FORWARD9(M, F), F(A1, A2, A3, A4, A5, A6, A7, A8, A9) -> M:F(A1, A2, A3, A4, A5, A6, A7, A8, A9)).

%% There's no equivalent of C preprocessor ## operator,
%% so no macro name concatenations possible.
%% This won't work.
%-define(FORWARD(M, F, A), ?(FORWARD ## A)(M, F)).
%?FORWARD(animal, does, 1).

%% This is as far as code reuse can be pushed without parse transforms.
?FORWARD1(animal, does).

says() -> "woof".
