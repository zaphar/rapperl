-module(rapperl_list, [ElementGen]).
-include("rapperl_constants.hrl").

-export([value/0]).

value() when is_list(ElementGen) ->
   [rapperl:pop(Gen) || Gen <- ElementGen];
value() ->
   value(rapperl:pop(rapperl:int(?DEFAULT_LIST_SIZE))).
value(0) ->
   [];
value(N) ->
   [rapperl:pop(ElementGen)|value(N - 1)].
