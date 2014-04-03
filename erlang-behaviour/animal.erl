-module(animal).

-callback says() -> string().

-callback does(atom() | string()) -> ok | {error, any()}.
