-module(rapperl_filter, [Gen, Pred]).
-include("rapperl_constants.hrl").
-export([value/0]).

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
