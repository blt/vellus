-module(vellus).
-include_lib("vellus/include/vellus.hrl").
-export([check/4, check/5]).

%% ===================================================================
%% API
%% ===================================================================

-spec check(mf(), [term()], return_check(), [property_fun()]) ->
                   ok | check_failure() | property_failure().
check({Module, Function}=MF, ArgGens, ReturnCheck, Properties) ->
    check({Module, Function}=MF, ArgGens, ReturnCheck, Properties, 100).

-spec check(mf(), [term()], return_check(), [property_fun()], pos_integer()) ->
                   true | check_failure() | property_failure().
check({_Module, _Function}, _ArgGens, _ReturnCheck, _Properties, 0) ->
    true;
check({Module, Function}=MF, ArgGens, ReturnCheck, Properties, Repeat)
  when
      is_atom(Module), is_atom(Function),
      is_list(ArgGens),
      is_function(ReturnCheck),
      is_list(Properties),
      is_integer(Repeat), Repeat > 0 ->
    Args = [A() || A <- ArgGens],
    case check_instance(MF, Args, ReturnCheck, Properties) of
        ok ->
            check(MF, ArgGens, ReturnCheck, Properties, Repeat-1);
        Other ->
            Other
    end.

%% ===================================================================
%% Private functions
%% ===================================================================

-spec check_instance(mf(), [term()], return_check(), [property_fun()]) ->
                            ok | check_failure() | property_failure().
check_instance({Module,Function}, Args, ReturnCheck, Properties)
  when
      is_atom(Module); is_atom(Function);
      is_list(Args);
      is_function(ReturnCheck);
      is_list(Properties) ->
    Return = apply(Module, Function, Args),
    case ReturnCheck(Return) of
        true ->
            run_props(Properties, Args);
        false ->
            {vellus, check_failure, Args}
    end.

-spec run_props([property_fun()], [term()]) -> ok | property_failure().
run_props([], _Args) ->
    ok;
run_props([Prop|Rest], Args) ->
    case Prop(Args) of
        true  ->
            run_props(Rest, Args);
        false ->
            {vellus, property_failure, Args}
    end.
