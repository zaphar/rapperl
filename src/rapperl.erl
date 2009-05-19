-module(rapperl).

-export([init/0]).

-export([int/0,
        list/1,
        tuple/1,
        filter_gen/2,
        check/2]).

check(Gen, Test) ->
	check(Gen, Test, 100).

check(Gen, Test, 0) ->
	io:format("~n", []),
	ok;
check(Gen, Test, N) ->
	Result = Test(Gen:value()),
	case Result of
		true ->
			io:format(".", []);
		false ->
			io:format("!", [])
	end,
	check(Gen, Test, N - 1).
			

init() ->
	{A, B, C} = now(),
	random:seed(A, B, C),
	ok.

gen_int() ->
	random:uniform(100).
shrink_int(Int) ->
	Int - 1.
int() ->
	rapperl_generator:new(fun gen_int/0, fun shrink_int/1).
	
gen_list(ElemGen) ->
	Size = gen_int(),
	Scaffold = lists:seq(1, Size),
	[ElemGen:value() || _ <- Scaffold].
shrink_list(List) ->
	[H|T] = List,
	T.
list(ElemGen) ->
	Generator = fun() -> gen_list(ElemGen) end,
	rapperl_generator:new(Generator, fun shrink_list/1).

gen_tuple({}) ->
	{};
gen_tuple({Gen}) ->
	{Gen:value()};
gen_tuple({Gen0, Gen1}) ->
	{Gen0:value(), Gen1:value()};
gen_tuple({Gen0, Gen1, Gen2}) ->
	{Gen0:value(), Gen1:value(), Gen2:value()}.
shrink_tuple(Unused) ->
	Unused.
tuple(GenTuple) ->
	Generator = fun() -> gen_tuple(GenTuple) end,
	rapperl_generator:new(Generator, fun shrink_tuple/1).

filter_gen(Gen, Pred) ->
	rapperl_filter:new(Gen, Pred).
