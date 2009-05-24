-module(rapperl_list, [ElementGen]).
-include("rapperl_constants.hrl").

-export([value/0
        ,shrink_strategies/0
        ,shrinks_with/2
        ,shrink/2]).

value() when is_list(ElementGen) ->
   [rapperl:pop(Gen) || Gen <- ElementGen];
value() ->
   value(rapperl:pop(rapperl:int(?DEFAULT_LIST_SIZE))).
value(0) ->
   [];
value(N) ->
   [rapperl:pop(ElementGen)|value(N - 1)].

shrink_strategies() ->
   [shrink_all,
    shrink_one,
    delete_first].

shrinks_with(shrink_all, []) ->
   false;
shrinks_with(shrink_one, []) ->
   false;
shrinks_with(delete_first, []) ->
   false;
shrinks_with(delete_first, _) when is_list(ElementGen) ->
   false;
shrinks_with(_, _) ->
   true.

shrink(List, shrink_all) when is_list(ElementGen) ->
   ElemWithGen = lists:zip(ElementGen, List),
   [rapperl:shrink(Gen, Elem) || {Gen, Elem} <- ElemWithGen];
% TODO - clean this up
shrink(List, shrink_one) when is_list(ElementGen) ->
   Index = index_in_list(List),
   Gen   = lists:nth(Index + 1, ElementGen),
   {Before, [Elem|After]} = lists:split(Index, List),
   lists:concat([Before, [rapperl:shrink(Gen, Elem)|After]]);

shrink(List, shrink_one) ->
   Index = index_in_list(List),
   {Before, [H|T]} = lists:split(Index, List),
   lists:concat([Before, [rapperl:shrink(ElementGen, H)|T]]);

shrink(List, shrink_all) ->
   [rapperl:shrink(ElementGen, Elem) || Elem <- List];

shrink([H|T], delete_first) ->
   T.

index_in_list(List) ->
   rapperl:pop(rapperl:int(0, length(List) - 1)).
