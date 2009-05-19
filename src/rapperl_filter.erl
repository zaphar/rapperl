-module(rapperl_filter, [Generator, Predicate]).

-export([value/0,
         shrink/1]).

value() ->
	value(false, undefined).

value(false, _Val) ->
	Val = Generator:value(),
	value(Predicate(Val), Val);

value(true, Val) ->
	Val.

shrink(Val) ->
	Generator:shrink(Val).
