-module(rapperl).

-export([init/0,
         check/2,
         check/3,
         sample/1,
         sample/2]).

-export([int/0,
         int/1,
         int/2,
         list/1,
         tuple/1,
         constant/1]).

-export([pick_one/1,
         filter_gen/2,
         execute/1]).

init() ->
	{A, B, C} = now(),
	random:seed(A, B, C),
	ok.

check(Gen, Test) ->
	io:format("Beginning Check~n", []),
	do_check(Gen, Test, 100).
check(Gen, Test, N) ->
	io:format("Beginning Check~n", []),
	do_check(Gen, Test, N).

do_check(Gen, Test, 0) ->
	io:format("~n", []),
	ok;
do_check(Gen, Test, N) ->
	Result = Test(Gen:value()),
	case Result of
		true ->
			io:format(".", []);
		false ->
			io:format("!", [])
	end,
	do_check(Gen, Test, N - 1).

sample(Generator) ->
	sample(10, Generator).
sample(Size, Generator) ->
	Scaffold = lists:seq(1, Size),
	[Generator:value() || _ <- Scaffold].

int() ->
	rapperl_int:new(0, 100).
int(To) ->
	rapperl_int:new(0, To).
int(From, To) ->
	rapperl_int:new(From, To).

list(ElemGen) ->
	rapperl_list:new(ElemGen).

tuple(GenTuple) ->
	rapperl_tuple:new(GenTuple).

constant(Val) ->
	rapperl_constant:new(Val).

filter_gen(Gen, Pred) ->
	rapperl_filter:new(Gen, Pred).

pick_one(ListGen) ->
	rapperl_pick_one:new(ListGen).

execute({call, M, F, A}) ->
	apply(M, F, [execute(Arg) || Arg <- A:value()]);
execute(Arg) ->
	Arg.
