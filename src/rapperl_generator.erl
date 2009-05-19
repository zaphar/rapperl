-module(rapperl_generator, [Generator, Shrink]).

-export([value/0,
         shrink/1]).

value() when is_function(Generator) ->
	Generator();
value() when is_tuple(Generator) and size(Generator) == 2 ->
	Generator:value().

shrink(Val) when is_function(Shrink) ->
	Shrink(Val);
shrink(Val) when is_tuple(Generator) and size(Generator) == 2 ->
	Shrink:shrink(Val).
