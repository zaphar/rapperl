-module(rapperl_int, [From, To]).

-export([value/0]).

%
% Produce a random values within the range this
% generator was instatiated with.
%
value() ->
   From + random:uniform(To - From).
