-module(rapperl_tuple, [ElementGen]).

-export([value/0,
         shrink/2,
         shrink_strategies/0]).

value() ->
	gen_val(ElementGen).

gen_val({}) ->
	{};
gen_val({Gen}) ->
	{Gen:value()};
gen_val({Gen0, Gen1}) ->
	{Gen0:value(), Gen1:value()};
gen_val({Gen0, Gen1, Gen2}) ->
	{Gen0:value(), Gen1:value(), Gen2:value()};
gen_val({Gen0, Gen1, Gen2, Gen3}) ->
	{Gen0:value(), Gen1:value(), Gen2:value(), Gen3:value()}.

shrink_strategies() ->
	[all_elements].

shrink({}, all_elements) ->
	{};
shrink({Val}, all_elements) ->
	{Gen} = ElementGen,
	{Gen:shrink(Val)};
shrink({Val0, Val1}, all_elements) ->
	{Gen0, Gen1} = ElementGen,
	{Gen0:shrink(Val0), Gen1:shrink(Val1)};
shrink({Val0, Val1, Val2}, all_elements) ->
	{Gen0, Gen1, Gen2} = ElementGen,
	{Gen0:shrink(Val0), Gen1:shrink(Val1), Gen2:shrink(Val2)};
shrink({Val0, Val1, Val2, Val3}, all_elements) ->
	{Gen0, Gen1, Gen2, Gen3} = ElementGen,
	{Gen0:shrink(Val0), Gen1:shrink(Val1), Gen2:shrink(Val2), Gen3:shrink(Val3)}.
