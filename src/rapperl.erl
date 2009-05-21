-module(rapperl).

-export([init/0,
         check/2,
         check/3,
			prepare/2,
			prepare/3,
         sample/1,
         sample/2]).

-export([int/0,
         int/1,
         int/2,
         list/1,
         tuple/1,
         constant/1]).

-export([pop/1,
         pick_one/1,
         filter_gen/2,
			map_gen/2,
         execute/1]).

-define(DEFAULT_TEST_COUNT,  1000).
-define(DEFAULT_SAMPLE_SIZE, 10).

init() ->
	{A, B, C} = now(),
	random:seed(A, B, C),
	ok.

check(Gen, Test) ->
	check(Gen, Test, ?DEFAULT_TEST_COUNT).
check(Gen, Test, N) ->
	io:format("Beginning Check~n", []),
	Start  = now(),
	Res = do_check(Gen, Test, N),
	Finish = now(),
	Diff   = timer:now_diff(Start, Finish),
	io:format("Ran ~w tests in ~wms ~n", [N, N / 1000]),
	Res.

do_check(Gen, Test, 0) ->
	io:format("~n", []),
	ok;
do_check(Gen, Test, N) ->
	Value = Gen:value(),
	Result = Test(Value),
	case Result of
		true ->
			io:format(".", []),
			do_check(Gen, Test, N - 1);
		false ->
			io:format("!", []),
			{failed, Value}
	end.

prepare(Gen, Test) ->
	fun() -> rapperl:check(Gen, Test) end.
prepare(Gen, Test, N) ->
	fun() -> rapperl:check(Gen, Test, N) end.

sample(Generator) ->
	sample(?DEFAULT_SAMPLE_SIZE, Generator).
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

map_gen(Gen, Fun) ->
	rapperl_map:new(Gen, Fun).

pop(Gen) ->
	Gen:value().

pick_one(ListGen) ->
	rapperl_pick_one:new(ListGen).

execute({call, M, F, A}) ->
	apply(M, F, [execute(Arg) || Arg <- A:value()]);
execute(Arg) ->
	Arg.
