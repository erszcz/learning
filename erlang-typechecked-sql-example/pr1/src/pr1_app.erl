-module(pr1_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([f/1, g/0, h/0, i/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    pr1_sup:start_link().

stop(_State) ->
    ok.

-type t() :: {escaped_sql, binary()}.

-spec f(t()) -> t().
f(T) -> T.

-spec make_binary() -> binary().
make_binary() -> <<"bin">>.

-spec make_t() -> t().
make_t() -> {escaped_sql, <<"t">>}.

g() -> f(make_binary()).

h() -> f(make_t()).

i() -> f(z).
