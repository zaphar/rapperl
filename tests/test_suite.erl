-module(test_suite).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

constant_test() ->
	?assert([0] == ordsets:from_list(rapperl:sample(100, rapperl:constant(0)))).

integer_test() ->
	?assert(is_integer(rapperl:pop(rapperl:int()))).

integer_upper_limit_member_test() ->
	Ints = ordsets:from_list(rapperl:sample(100, rapperl:int(10))),
	?assert(ordsets:is_element(10, Ints)).

integer_upperl_limit_range_test() ->
   Ints  = ordsets:from_list(rapperl:sample(100, rapperl:int(10))),
   Range = lists:seq(0, 10),
   ?assertEqual(Range, Ints).
   
integer_lower_and_upper_limit_members_test() ->
	Ints = ordsets:from_list(rapperl:sample(100, rapperl:int(5, 15))),
	?assert(ordsets:is_element(5, Ints)),
	?assert(ordsets:is_element(5, Ints)).

integer_lower_and_upper_limit_range_test() ->
   Ints  = ordsets:from_list(rapperl:sample(100, rapperl:int(5, 15))),
   Range = lists:seq(5, 15),
   ?assertEqual(Range, Range).

integer_negative_lower_limit_member_test() ->
	Ints = ordsets:from_list(rapperl:sample(100, rapperl:int(-10))),
	?assert(ordsets:is_element(-10, Ints)),
	?assert(ordsets:is_element(0, Ints)).

integer_negative_lower_limit_range_test() ->
	Ints  = ordsets:from_list(rapperl:sample(100, rapperl:int(-10))),
	Range = lists:seq(-10, 0),
	?assertEqual(Range, Ints).

integer_negative_lower_limit_positive_upper_limit_members_test() ->
	Ints = ordsets:from_list(rapperl:sample(100, rapperl:int(-5, 5))),
	?assert(ordsets:is_element(-5, Ints)),
	?assert(ordsets:is_element(5, Ints)).

integer_negative_lower_limit_positive_upper_limit_range_test() ->
	Ints  = ordsets:from_list(rapperl:sample(100, rapperl:int(-5, 5))),
	Range = lists:seq(-5, 5),
	?assertEqual(Range, Ints).

integer_negative_lower_limit_negative_upper_limit_members_test() ->
	Ints = ordsets:from_list(rapperl:sample(100, rapperl:int(-15, -5))),
	?assert(ordsets:is_element(-15, Ints)),
	?assert(ordsets:is_element(-5, Ints)).

integer_negative_lower_limit_negative_upper_limit_range_test() ->
	Ints  = ordsets:from_list(rapperl:sample(100, rapperl:int(-15, -5))),
	Range = lists:seq(-15, -5),
	?assertEqual(Range, Ints).

fixed_list_size_test_() ->
	None = rapperl:list([]),
	One  = rapperl:list([rapperl:int()]),
	Two  = rapperl:list([rapperl:int(), rapperl:int()]),

   [?_assertEqual(0, length(rapperl:pop(None)))
   ,?_assertEqual(1, length(rapperl:pop(One)))
   ,?_assertEqual(2, length(rapperl:pop(Two)))].

fixed_list_elements_test() ->
   List = rapperl:pop(
             rapperl:list([
                rapperl:constant(first),
                rapperl:constant(second)])),
   ?assertEqual([first, second], List).

dynamic_list_members_test() ->
   Samples   = rapperl:sample(rapperl:list(rapperl:int(10))),
	UniqElems = ordsets:from_list(lists:flatten(Samples)),
   Range     = lists:seq(0, 10),
   ?assertEqual(Range, UniqElems).

tuple_elements_test() ->
   Tuple = rapperl:pop(
              rapperl:tuple({
                 rapperl:constant(first),
                 rapperl:constant(second)})),
   ?assertEqual({first, second}, Tuple).

filter_test() ->
	Sample = rapperl:sample(3,
               rapperl:filter_gen(
                  rapperl:int(1),
                  fun(Int) -> Int == 0 end)),
   ?assertEqual([0, 0, 0], Sample).

map_test() ->
	Sample = rapperl:sample(10,
               rapperl:map_gen(
                  rapperl:int(1,2),
                  fun(Int) -> Int*2 end)),
	Unique = ordsets:from_list(Sample),
   ?assertEqual([{1, 2},{2, 4}], Unique).

successful_check_test() ->
   Result = rapperl:check(
               rapperl:constant('_'),
               fun(_) -> true end),
   ?assertEqual(ok, Result).

failing_check_test() ->
   Result = rapperl:check(
               rapperl:constant('_'),
               fun(_) -> false end),
   ?assertMatch({error, _}, Result).

successful_prepared_check_test() ->
   Check = rapperl:prepare(
              rapperl:constant('_'),
              fun(_) -> true end),
   ?assertEqual(ok, Check()).

failing_prepared_check_test() ->
   Check = rapperl:prepare(
              rapperl:constant('_'),
              fun(_) -> false end),
   ?assertMatch({error, _}, Check()).

