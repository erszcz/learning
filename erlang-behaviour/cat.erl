-module(cat).

-behaviour(animal).

-export([says/0,
         does/1]).

-spec says() -> string().
says() -> "meow".

-spec does(any()) -> ok | {error, any()}.
does(_) -> ok.
