-module(rapperl_list, [ElementGen]).

-export([value/0]).

value() when is_list(ElementGen) ->
	[rapperl:pop(Gen) || Gen <- ElementGen];
value() ->
	value(rapperl:pop(rapperl:int())).
value(0) ->
	[];
value(N) ->
	[rapperl:pop(ElementGen)|value(N - 1)].	
