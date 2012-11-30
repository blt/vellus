-type return_check() :: fun((term()) -> boolean()).
-type property_fun() :: fun(([term()]) -> boolean()).
-type mf()           :: {atom(), atom()}.

-record(property_failure, {
          type        :: exception | logical,
          arguments   :: [term()],
          value       :: term(), %% exists only for type =:= logical
          exception   :: {atom(), atom()} %% exists only for type =:= exception
         }
       ).
-record(check_failure, {
          type :: exception | logical,
          mf   :: mf(),
          arguments :: [term()],
          value :: term(), %% exists only for type =:= logical
          exception :: {atom(), atom()} %% exists only for type =:= exception
         }
       ).
