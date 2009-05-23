-module(rapperl).

-include("rapperl_interfaces.hrl").
-export([init/0,
         check/2,
         check/3,
			prepare/2,
			prepare/3,
         sample/1,
         sample/2,
         pop/1]).

-export([int/0,
         int/1,
         int/2,
         list/1,
         tuple/1,
         constant/1,
         filter_gen/2,
			map_gen/2]).

-define(DEFAULT_TEST_COUNT,  1000).
-define(DEFAULT_SAMPLE_SIZE, 10).

%
% Setup environment
%
init() ->
	{A, B, C} = now(),
	random:seed(A, B, C),
	ok.

%
% Perform check
%
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

%
% Prepare a check as an anymous function returning the result of the check
%
prepare(Gen, Test) ->
	fun() -> rapperl:check(Gen, Test) end.
prepare(Gen, Test, N) ->
	fun() -> rapperl:check(Gen, Test, N) end.

%
% Produce value from generator
%
pop(Gen) ->
	Gen:value().

%
% Sample a generator
%
sample(Generator) ->
	sample(?DEFAULT_SAMPLE_SIZE, Generator).
sample(Size, Generator) ->
	Scaffold = lists:seq(1, Size),
	[Generator:value() || _ <- Scaffold].

%
% Integer generator instantiators
%
int() ->
	rapperl_int:new(0, 100).
int(To) ->
	rapperl_int:new(0, To).
int(From, To) ->
	rapperl_int:new(From, To).

%
% List generator instantiator
%
list(ElemGen) ->
	rapperl_list:new(ElemGen).

%
% Tuple generator instantiator
%
tuple(GenTuple) ->
	rapperl_tuple:new(GenTuple).
%
% Constant instatiator 
%
constant(Val) ->
	rapperl_constant:new(Val).
%
% Filtering generator instatiator
%
filter_gen(Gen, Pred) ->
	rapperl_filter:new(Gen, Pred).

%
% Transforming generator instatiator
%
map_gen(Gen, Fun) ->
	rapperl_map:new(Gen, Fun).
