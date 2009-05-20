-module(test_suite).
-include_lib("eunit/include/eunit.hrl").

constant_test() ->
	ok == rapperl:check(
	         rapperl:constant(0),
            fun(0) -> true;
               (_) -> false end).

not_constant_test() ->
	not ok == rapperl:check(
	             rapperl:constant(1),
                fun(0) -> true;
                   (_) -> false end).
                
		
