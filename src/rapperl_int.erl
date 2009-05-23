-module(rapperl_int, [From, To]).

-export([value/0]).

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
