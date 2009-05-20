-module(rapperl_int, [From, To]).

-export([value/0,
         shrink/2,
         shrink_strategies/0]).

value() ->
	From + random:uniform(To - From).

shrink_strategies() ->
	[decrease,
    inscrease].

shrink(Int, decrease) ->
	Int - 1;
shrink(Int, inscrease) ->
	Int + 1.
