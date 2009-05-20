-module(test_suite).
-include_lib("eunit/include/eunit.hrl").

successful_test() ->
   ok == rapperl:check(
            rapperl:constant('_'),
		      fun(_) -> true end).

failing_test() ->
   ok /= rapperl:check(
	         rapperl:constant('_'),
				fun(_) -> false end).

constant_test() ->
	ok == rapperl:check(
	         rapperl:constant(0),
            fun(0) -> true;
               (_) -> false end).

not_constant_test() ->
	ok /= rapperl:check(
	         rapperl:constant(1),
            fun(0) -> true;
               (_) -> false end).

integer_test() ->
   ok == rapperl:check(
	         rapperl:int(),
				fun(Val) -> is_integer(Val) end).

integer_upper_limit_test() ->
   ok == rapperl:check(
            rapperl:int(10),
            fun(Val) -> is_integer(Val)
				            and (Val =< 10) end).

integer_lower_and_upper_limit_test() ->
   ok == rapperl:check(
	         rapperl:int(10, 20),
				fun(Val) -> is_integer(Val)
				            and (Val =< 20)
								and (Val >= 10) end).

integer_negative_lower_limit_test() ->
   ok == rapperl:check(
	         rapperl:int(-10, 10),
				fun(Val) -> is_integer(Val)
				            and (Val =< 0)
								and (Val >= -10) end).

filter_test() ->
	Pred = fun({Elem, List}) -> lists:member(Elem, List) end,
   ok == rapperl:check(
      rapperl:filter_gen(
         rapperl:tuple({rapperl:int(10),
                        rapperl:list(rapperl:int(10))}),
			Pred),
		Pred).

pick_test() ->
   ok == rapperl:check(
      rapperl:list(rapperl:int()),
		fun(Picks) ->
		   PickGen = rapperl:pick_one(rapperl:constant(Picks)),
			Pick    = PickGen:value(),
		   lists:member(Pick, Picks)
      end).

sample_test() ->
	Sample = rapperl:sample(10, rapperl:int()),
   10 == length(Sample).

empty_sample_test() ->
	Sample = rapperl:sample(0, rapperl:int()),
	0 == length(Sample).
