-module(test_suite).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all_test_() ->
	[?_assert(ok == successful())
	,?_assert(ok /= failing())
	,?_assert(ok == integer())
	,?_assert(ok == integer_upper_limit())
	,?_assert(ok == integer_lower_and_upper_limit())
	,?_assert(ok == integer_negative_lower_limit())
	,?_assert(ok == filter())
	,?_assert(ok == map_check())
	,?_assert(ok == pick())
	,?_assert(sample())
	,?_assert(empty_sample())
	,?_assert(ok == prepare())].

successful() ->
   rapperl:check(
            rapperl:constant('_'),
		      fun(_) -> true end).

failing() ->
   rapperl:check(
	         rapperl:constant('_'),
				fun(_) -> false end).

constant() ->
	rapperl:check(
	         rapperl:constant(0),
            fun(0) -> true;
               (_) -> false end).

not_constant() ->
	rapperl:check(
	         rapperl:constant(1),
            fun(0) -> true;
               (_) -> false end).

integer() ->
   rapperl:check(
	         rapperl:int(),
				fun(Val) -> is_integer(Val) end).

integer_upper_limit() ->
   rapperl:check(
            rapperl:int(10),
            fun(Val) -> is_integer(Val)
				            and (Val =< 10) end).

integer_lower_and_upper_limit() ->
   rapperl:check(
	         rapperl:int(10, 20),
				fun(Val) -> is_integer(Val)
				            and (Val =< 20)
								and (Val >= 10) end).

integer_negative_lower_limit() ->
   rapperl:check(
	         rapperl:int(-10, 10),
				fun(Val) -> is_integer(Val)
				            and (Val =< 10)
								and (Val >= -10) end).

filter() ->
	Pred = fun({Elem, List}) -> lists:member(Elem, List) end,
   rapperl:check(
      rapperl:filter_gen(
         rapperl:tuple({rapperl:int(10),
                        rapperl:list(rapperl:int(10))}),
			Pred),
		Pred).

pick() ->
   rapperl:check(
      rapperl:list(rapperl:int()),
		fun(Picks) ->
		   Pick = rapperl:pop(rapperl:pick_one(rapperl:constant(Picks))),
		   lists:member(Pick, Picks)
      end).

sample() ->
	Sample = rapperl:sample(10, rapperl:int()),
   10 == length(Sample).

empty_sample() ->
	Sample = rapperl:sample(0, rapperl:int()),
	0 == length(Sample).


prepare() ->
   Check = rapperl:prepare(
	           rapperl:constant(0),
              fun(0) -> true end),
   Check().

map_check() ->
	rapperl:check(
	   rapperl:map_gen(
		   rapperl:int(),
			fun(Int) -> -Int end),
		fun({Org, Negated}) -> Negated  == -Org end).

dict_element() ->
	rapperl:tuple({
	   rapperl:int(),
	   rapperl:int()}).

dict() ->
   rapperl:map_gen(
      rapperl:list(dict_element()),
		fun(DictList) ->
		   dict:from_list(DictList)
		end).

dict_check() ->
	rapperl:check(
	   dict(),
		fun({_, Dict}) ->
			AsList0   = dict:to_list(Dict),
			FromList1 = dict:from_list(AsList0),
			AsList1   = dict:to_list(FromList1),
         lists:sort(AsList0) == lists:sort(AsList1)
		end).


