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
   [shrink_all, delete_first].

shrinks_with(shrink_all, []) ->
   false;
shrinks_with(delete_first, []) ->
   false;
shrinks_with(_, _) ->
   true.

shrink(List, shrink_all) ->
   [rapperl:shrink(ElementGen, Elem) || Elem <- List];
shrink([H|T], delete_first) ->
   T.
