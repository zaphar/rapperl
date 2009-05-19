-module(rapperl_test).

-export([test/0]).



test() ->
	rapperl:check(
		rapperl:filter_gen(
			rapperl:tuple({rapperl:int(), rapperl:list(rapperl:int())}),
			fun({Elem, List}) -> lists:member(Elem, List) end),
			
			fun({Elem, ListWithElem}) ->
				OriginalSize = length(ListWithElem),
				NewList      = lists:delete(Elem, ListWithElem),
				NewSize      = length(NewList),
				NewSize < OriginalSize end).
