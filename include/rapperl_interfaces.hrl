-type generator() :: module().
-type test_function() :: fun((any()) -> bool()).
-type check_result()  :: 'ok' | {error, atom()}.
-type test_count()    :: pos_integer().
-type sample_result() :: list(any()).

-spec rapperl:check(generator(), test_function()) -> check_result().
-spec rapperl:check(test_count(), generator(), test_function()) -> check_result().

-spec rapperl:prepare(generator(), test_function()) -> fun((none()) -> check_result()).
-spec rapperl:prepare(test_count(), generator(), test_function()) -> fun((none()) -> check_result()).

-spec rapperl:sample(generator()) -> sample_result().
-spec rapperl:sample(test_count(), generator()) -> sample_result().

-spec rapperl:pop(generator()) -> any().

-spec rapperl:int() -> generator().
-spec rapperl:int(integer()) -> generator().
-spec rapperl:int(integer(), integer()) -> generator().

-spec rapperl:list(generator() | list(generator())) -> generator().

-spec rapperl:tuple(tuple()) -> generator().

-spec rapperl:constant(any()) -> generator().

-spec rapperl:filter_gen(generator(), test_function()) -> generator().
-spec rapperl:map_gen(generator(), fun((any()) -> {any(), any()})) -> generator().
