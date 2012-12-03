# Vellus - a stochastic erlang testing library

Vellus is a library meant to make stochastic testing in erlang natural in eunit
and common test setups. You give `vellus:check/4` the function, argument
generators, a return type check and properties you'd like to always hold and
vellus will try and make your function fail.

## A quick example

Here's an ordinary arithmatic division function that returns `singularity` when
the denominator is 0.

```erlang
-module(divtest).

-spec divide(integer(), integer()) -> float() | singularity.
divide(N, 0) when is_integer(N) ->
    singularity;
divide(N, D) when is_integer(N), is_integer(D) ->
    N / D.
```

It has the following properties:

  * return must always be a floating point or singularity
  * return must be singularity when D =:= 0
  * D/N must not be N/D unless D == N
  * D/D == 1.0

The return type check is:

```erlang
return_check(Val) ->
    is_float(Val) or (Val =:= singularity)
end
```

The property check function is:

```erlang
divide_properties([N,0]) ->
    divide(D,0) =:= singularity;
divide_properties([N,D]) when N =/= D ->
    divide(N,D) =/= divide(D,N);
divide_properties([N,N]) when N =/= 0 ->
    divide(N,N) =:= 1.0
end
```

Now, where do we get random integers? Vellus provides instance generators for
some types in `vellus_random`--we will use `vellus_random:pos_integer/0`--but if
the generators provided are insufficient for your needs take heart, there's no
special magic to rolling your own: randomly return an instance of the type you
need from a 0-arity function.

There is now enough to run `divide/2` through vellus, say over 10000 pairs of
integers:

```erlang
divide_test_() ->
    ArgumentGenerator = [fun vellus_random:pos_integer/0, fun vellus_random:pos_integer/0],
    ?_assert(vellus:check({?MODULE, divide}, ArgumentGenerator, fun return_check/1,
                         [ fun divide_properties/1 ], 10000)).
```

That's that. Please see `test/` for more examples of vellus in action and read
`src/vellus.erl` for implementation details.

Happy hacking!
