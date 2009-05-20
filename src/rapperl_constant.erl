-module(rapperl_constant, [ConstantValue]).

-export([value/0,
         shrink/1]).

% Always same value
value() ->
	ConstantValue.

% Value is constant, don't shrink
shrink(Value) ->
	Value.
