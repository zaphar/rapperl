-module(rapperl).
-include("rapperl_interfaces.hrl").
-include("rapperl_constants.hrl").

-export([init/0,
         check/2,
         check/3,
         prepare/2,
         prepare/3,
         sample/1,
         sample/2,
         sample_unique/1,
         sample_unique/2,
         pop/1]).

-export([int/0,
         int/1,
         int/2,
         list/1,
         tuple/1,
         constant/1,
         filter_gen/2,
         map_gen/2]).

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
   Res = do_check(Gen, Test, N),
   io:format("~nRan ~w tests~n", [N]),
   Res.

do_check(Gen, Test, 0) ->
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
         {error, Value}
   end.

%
% Prepare a check as an anymous function returning the result of the check
%
prepare(Gen, Test) ->
   fun() -> rapperl:check(Gen, Test) end.
prepare(Gen, Test, N) ->
   fun() -> rapperl:check(Gen, Test, N) end.

%
% Produce a value from generator
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
   Sample   = [Generator:value() || _ <- Scaffold],
	lists:sort(Sample).

%
% Sample a generator and return unique values
%
sample_unique(Generator) ->
	ordsets:from_list(sample(Generator)).
sample_unique(Size, Generator) ->
	ordsets:from_list(sample(Size, Generator)).

%
% Integer generator instantiators
%
int() ->
   rapperl_int:new(0, ?DEFAULT_MAX_INT).
int(To) when To < 0 ->
   rapperl_int:new(To, 0);
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
