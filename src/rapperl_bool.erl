-module(rapperl_bool, [Val, In, OutOf]).

-export([value/0]).

value() when Val == true ->
   Prob = rapperl:pop(rapperl:int(1, OutOf)),
   Prob =< In;
value() when Val == false ->
   Prob = rapperl:pop(rapperl:int(1, OutOf)),
   not (Prob =< In).
