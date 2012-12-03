-module(vellus_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("vellus/include/vellus.hrl").
-export([divide/2]).

%% ===================================================================
%% API
%% ===================================================================

-spec divide(integer(), integer()) -> float() | singularity.
divide(N, 0) when is_integer(N) ->
    singularity;
divide(N, D) when is_integer(N), is_integer(D) ->
    N / D.

%% ===================================================================
%% Test functions
%% ===================================================================

-define(ARGS, [fun vellus_random:pos_integer/0, fun vellus_random:pos_integer/0]).

divide_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     {timeout, 180,
      [
       ?_assertMatch({vellus, check_failure, _},
                     vellus:check({?MODULE, divide},
                                  ?ARGS, fun is_integer/1,
                                  [ fun divide_properties/1 ]
                                 )
                    ),
       ?_assertException(throw, test,
                         vellus:check({?MODULE, divide},
                                      ?ARGS, fun(_) -> erlang:throw(test) end,
                                      [ fun divide_properties/1 ]
                                     )
                         ),
       ?_assertMatch({vellus, property_failure, _},
                     vellus:check({?MODULE, divide},
                                  ?ARGS, fun return_check/1,
                                  [ fun(_) -> false end ]
                                 )
                    ),
       ?_assertException(throw, test,
                         vellus:check({?MODULE, divide},
                                      ?ARGS, fun return_check/1,
                                      [ fun(_) -> erlang:throw(test) end ]
                                     )
                        ),
       ?_assert(vellus:check({?MODULE, divide},
                                  ?ARGS, fun return_check/1,
                                  [ fun divide_properties/1 ]
                                 )),
       ?_assert(vellus:check({?MODULE, divide},
                             ?ARGS, fun return_check/1,
                             [ fun divide_properties/1 ],
                             1000))
      ]
     }
    }.

%% ===================================================================
%% Internal functions
%% ===================================================================

return_check(Val) ->
    is_float(Val) or (Val =:= singularity).

divide_properties([N,0]) ->
    divide(N,0) =:= singularity;
divide_properties([N,D]) when N =/= D ->
    divide(N,D) =/= divide(D,N);
divide_properties([N,N]) when N =/= 0 ->
    divide(N,N) =:= 1.0.

start() -> ok.
stop(_) -> ok.
