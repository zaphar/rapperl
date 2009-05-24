-module(rapperl_filter, [Gen, Pred]).
-include("rapperl_constants.hrl").
-export([value/0
        ,shrink_strategies/0
        ,shrinks_with/2
        ,shrink/2]).

value() ->
   Val = rapperl:pop(Gen),
   value(?DEFAULT_MAX_TRIES, Pred(Val), Val).

value(_, true, Val) ->
   Val;
value(0, _, _) ->
   throw(too_many_tries);
value(N, false, _) ->
   Val = rapperl:pop(Gen),
   value(N - 1, Pred(Val), Val).

shrink_strategies() ->
   Gen:shrink_strategies().

shrinks_with(Strategy, Value) ->   
   Gen:shrinks_with(Strategy, Value).

shrink(Value, Strategy) ->
   NewVal = Gen:shrink(Value, Strategy),
   case Pred(NewVal) of
      true ->
         NewVal;
      false ->
         Value
   end.
