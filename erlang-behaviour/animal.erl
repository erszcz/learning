-module(animal).

-callback says() -> string().
-callback does(atom() | string()) -> ok | {error, any()}.

-export([says/0,
         does/1]).

says() -> "nothing".

does(_) -> ok.
