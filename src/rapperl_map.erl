-module(rapperl_map, [Gen, Fun]).

-export([value/0
        ,shrink_strategies/0
        ,shrinks_with/2
        ,shrink/2]).

value() ->
   Val = rapperl:pop(Gen),
   {Val, Fun(Val)}.

shrink_strategies() ->
   Gen:shrink_strategies().

shrinks_with(Strategy, {OrgVal, _}) ->
   Gen:shrinks_with(Strategy, OrgVal).

shrink({OrgVal, _}, Strategy) ->
   NewVal = Gen:shrink(OrgVal, Strategy),
   {NewVal, Fun(NewVal)}.
   
