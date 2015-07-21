-module(fmt).

-compile([export_all]).

-export_type([fmt/0]).

%% In fact this should be {module(), any(), any(), ...} with undefined number of any()s.
-type fmt() :: any().

-callback to_str() -> list().

to_str(Val) -> (element(1, Val)):to_str().
