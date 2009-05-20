-module(rapperl_list, [ElementGen]).

-export([value/0,
         shrink/2,
         shrink_strategies/0]).

value() when is_list(ElementGen) ->
	[Element:value() || Element <- ElementGen];
value() ->
	SizeGen = rapperl:int(),
	Size    = SizeGen:value(),
	value(Size).
value(0) ->
	[];
value(N) ->
	[ElementGen:value()|value(N - 1)].	

shrink_strategies() ->
	[delete_one,
    shrink_one,
	 shrink_all]

shrink(List, delete_one) ->
	[H|T] = List,
	T;
shrink(List, shrink_one) when not is_list(ElementGen) ->
	[H|T] = List,
	[ElementGen:shrink(H)|T];
shrink(List, shrink_all) when not is_list(ElementGen) ->
	[ElementGen:shrink(Elem) || Elem <- List.
