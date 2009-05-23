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
   ?assert(Ints == Range).
   
integer_lower_and_upper_limit_members_test() ->
	Ints = ordsets:from_list(rapperl:sample(100, rapperl:int(5, 15))),
	LowerIsMember = ordsets:is_element(5, Ints),
	UpperIsMember = ordsets:is_element(5, Ints),
	?assert(LowerIsMember and UpperIsMember).

integer_lower_and_upper_limit_range_test() ->
   Ints  = ordsets:from_list(rapperl:sample(100, rapperl:int(5, 15))),
   Range = lists:seq(5, 15),
   ?assert(Ints == Range).

integer_negative_lower_limit_member_test() ->
	Ints = ordsets:from_list(rapperl:sample(100, rapperl:int(-10))),
	LowerIsMember = ordsets:is_element(-10, Ints),
	UpperIsMember = ordsets:is_element(0, Ints),
	?assert(LowerIsMember and UpperIsMember).

integer_negative_lower_limit_range_test() ->
	Ints  = ordsets:from_list(rapperl:sample(100, rapperl:int(-10))),
	Range = lists:seq(-10, 0),
	?assert(Ints == Range).

integer_negative_lower_limit_positive_upper_limit_members_test() ->
	Ints = ordsets:from_list(rapperl:sample(100, rapperl:int(-5, 5))),
	LowerIsMember = ordsets:is_element(-5, Ints),
	UpperIsMember = ordsets:is_element(5, Ints),
	?assert(LowerIsMember and UpperIsMember).

integer_negative_lower_limit_positive_upper_limit_range_test() ->
	Ints  = ordsets:from_list(rapperl:sample(100, rapperl:int(-5, 5))),
	Range = lists:seq(-5, 5),
	?assert(Ints == Range).

integer_negative_lower_limit_negative_upper_limit_members_test() ->
	Ints = ordsets:from_list(rapperl:sample(100, rapperl:int(-15, -5))),
	LowerIsMember = ordsets:is_element(-15, Ints),
	UpperIsMember = ordsets:is_element(-5, Ints),
	?assert(LowerIsMember and UpperIsMember).

integer_negative_lower_limit_negative_upper_limit_range_test() ->
	Ints  = ordsets:from_list(rapperl:sample(100, rapperl:int(-15, -5))),
	Range = lists:seq(-15, -5),
	?assert(Ints == Range).


