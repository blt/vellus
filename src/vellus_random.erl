-module(vellus_random).

-export([pos_integer/0]).

-spec pos_integer() -> pos_integer().
pos_integer() ->
    trunc(random:uniform() * 10000).
