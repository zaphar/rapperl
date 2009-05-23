-module(rapperl_map, [Gen, Fun]).

-export([value/0]).

value() ->
	Val = rapperl:pop(Gen),
	{Val, Fun(Val)}.
