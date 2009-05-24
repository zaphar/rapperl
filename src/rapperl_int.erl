-module(rapperl_int, [From, To]).

-export([value/0
        ,shrink_strategies/0
        ,shrinks_with/2
        ,shrink/2]).

%
% Produce a random values within the range this
% generator was instatiated with.
%
value() ->
   adjust(random:uniform(range_span())).

range_span() ->
   (To - From) + 1.

adjust(Val) ->
   (Val - 1) + From.

% Integers shrink towards zero
shrink_strategies() ->
	[increase, decrease].

shrinks_with(increase, Int) when Int < 0 ->
   true;
shrinks_with(decrease, Int) when Int > 0 ->
   true;
shrinks_with(_, _) ->
   false.

shrink(Int, increase) ->
   Int + 1;
shrink(Int, decrease) ->
   Int - 1.
