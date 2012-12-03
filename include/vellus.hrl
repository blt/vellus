-type return_check() :: fun((term()) -> boolean()).
-type property_fun() :: fun(([term()]) -> boolean()).
-type mf()           :: {atom(), atom()}.

-type check_failure()    :: {vellus, check_failure, [term()]}.
-type property_failure() :: {vellus, property_failure, [term()]}.
