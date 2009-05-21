-module(rapperl_pick_one, [ListGen]).

-export([value/0,
         shrink/1]).

value() ->
	List    = ListGen:value(),
	ListLen = length(List),
	Index   = rapperl:pop(rapperl:int(0, ListLen)),
	Element = lists:nth(Index, List).
	
shrink(Value) ->
	Value.
