-module(rapperl_bool, [Val, In, OutOf]).

-export([value/0
        ,shrink_strategies/0
        ,shrinks_with/2
        ,shrink/2]).

value() when Val == true ->
   Prob = rapperl:pop(rapperl:int(1, OutOf)),
   Prob =< In;
value() when Val == false ->
   Prob = rapperl:pop(rapperl:int(1, OutOf)),
   not (Prob =< In).

% Booleans shrink to false

shrink_strategies() ->
   [make_false].

shrinks_with(make_false, true) ->
   true;
shrinks_with(make_false, false) ->
   false.

shrink(true, make_false) ->
   false.
