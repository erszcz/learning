-module(badcat).

-behaviour(animal).

-export([says/0,
         does/1]).

-spec says() -> integer().
says() -> "meow".

-spec does(any()) -> string().
does(_) -> "sleep".
