-module(vellus).
-include_lib("vellus/include/vellus.hrl").
-export([check/4, check/5]).

%% ===================================================================
%% API
%% ===================================================================

-spec check(mf(), [term()], return_check(), [property_fun()]) ->
                   true | #property_failure{} | #check_failure{}.
check({Module, Function}=MF, ArgGens, ReturnCheck, Properties) ->
    check({Module, Function}=MF, ArgGens, ReturnCheck, Properties, 100).

-spec check(mf(), [term()], return_check(), [property_fun()], pos_integer()) ->
                   true | #property_failure{} | #check_failure{}.
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
        true ->
            check(MF, ArgGens, ReturnCheck, Properties, Repeat-1);
        Other ->
            Other
    end.

%% ===================================================================
%% Private functions
%% ===================================================================

-spec check_instance(mf(), [term()], return_check(), [property_fun()]) ->
                            true | #property_failure{} | #check_failure{}.
check_instance({Module,Function}=MF, Args, ReturnCheck, Properties)
  when
      is_atom(Module); is_atom(Function);
      is_list(Args);
      is_function(ReturnCheck);
      is_list(Properties) ->
    Return = apply(Module, Function, Args),
    try ReturnCheck(Return) of
        true ->
            run_props(Properties, Args);
        Other ->
            #check_failure{type=logical, mf=MF, arguments=Args, value=Other}
    catch
        Class:Exception ->
            #check_failure{type=exception, mf=MF, arguments=Args, exception={Class,Exception}}
    end.

-spec run_props([property_fun()], [term()]) -> true | #property_failure{}.
run_props([], _Args) ->
    true;
run_props([Prop|Rest], Args) ->
    try Prop(Args) of
        true ->
            run_props(Rest, Args);
        Other ->
            #property_failure{type=logical, arguments=Args, value=Other}
    catch
        Class:Exception ->
            #property_failure{type=exception, arguments=Args, exception={Class, Exception}}
    end.
