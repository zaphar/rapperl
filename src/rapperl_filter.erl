-module(rapperl_filter, [Generator, Predicate]).

-export([value/0,
         shrink/2,
         shrink_strategies/0]).

value() ->
	value(false, undefined).

value(false, undefined) ->
	Val = Generator:value(),
	value(Predicate(Val), Val);
value(false, _Val) ->
	io:format("x", []),
	Val = Generator:value(),
	value(Predicate(Val), Val);
value(true, Val) ->
	Val.

shrink_strategies() ->
	Generator:shrink_strategies().	

shrink(Val, Strategy) ->
	Generator:shrink(Val, Strategy).
