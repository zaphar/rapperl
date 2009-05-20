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
				NewSize < OriginalSize end),

	rapperl:check(
		rapperl:filter_gen(
			rapperl:tuple({rapperl:constant(5), rapperl:list(rapperl:int(10))}),
			fun({Elem, List}) -> lists:member(Elem, List) end),
			
			fun({Elem, ListWithElem}) ->
				lists:member(5, ListWithElem) end),

	rapperl:sample(100,
		rapperl:pick_one(
			rapperl:list(rapperl:int()))),

	rapperl:sample(
		rapperl:pick_one(
			rapperl:constant([first, second, third]))),

	rapperl:execute({call, orddict, from_list,
		rapperl:list([rapperl:list(
			rapperl:tuple(
				{rapperl:int(), rapperl:int()}))])}),

	rapperl:execute(queue()).

queue() ->
	rapperl:pick_one(
		rapperl:list([
			rapperl:constant({call, queue, new, []}),
			rapperl:tuple({
				rapperl:constant(call),
				rapperl:constant(queue),
				rapperl:list([
					rapperl:int(),
					queue()])})])).
