-module(rapperl_constant, [ConstantValue]).

-export([value/0]).

%
% Always produce the value this generator was instatiated with.
%
value() ->
   ConstantValue.
