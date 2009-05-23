-module(rapperl_filter, [Gen, Pred]).

-export([value/0]).

value() ->
	Val = rapperl:pop(Gen),
	value(Pred(Val), Val).
value(false, _) ->
	Val = rapperl:pop(Gen),
	value(Pred(Val), Val);
value(true, Val) ->
	Val.
