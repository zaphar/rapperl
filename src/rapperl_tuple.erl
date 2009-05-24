-module(rapperl_tuple, [TupleGen]).
-export([value/0
        ,shrink_strategies/0
        ,shrinks_with/2
        ,shrink/2]).

value() ->
   Size  = size(TupleGen),
   Empty = erlang:make_tuple(Size, undefined),
   gen_tuple(Empty, 1, Size).

gen_tuple(_, _, 0) ->
   {};
gen_tuple(Tuple, Last, Last) ->
   ElemGen  = element(Last, TupleGen),
   NewElem  = rapperl:pop(ElemGen),
   setelement(Last, Tuple, NewElem);
gen_tuple(Tuple, Index, Last) ->
   ElemGen  = element(Index, TupleGen),
   NewElem  = rapperl:pop(ElemGen),
   NewTuple = setelement(Index, Tuple, NewElem),
   gen_tuple(NewTuple, Index + 1, Last).

shrink_strategies() ->
   [shrink_all
   ,shrink_one].

shrinks_with(shrink_one, {}) ->
   false;
shrinks_with(shrink_all, {}) ->
   false;
shrinks_with(_, _) ->
   true.

shrink({Elem}, shrink_one) ->
   {Gen} = TupleGen,
   {rapperl:shrink(Gen, Elem)};   
shrink(Tuple, shrink_one) ->
   Index   = rapperl:pop(rapperl:int(1, size(Tuple))),
   Elem    = element(Index, Tuple),   
   ElemGen = element(Index, TupleGen),
   NewElem = rapperl:shrink(ElemGen, Elem),
   setelement(Index, Tuple, NewElem);

shrink({Val}, shrink_all) ->
   {Gen} = TupleGen,
   {rapperl:shrink(Gen, Val)};

shrink(Tuple, shrink_all) ->
   shrink_all(Tuple, 1, size(Tuple)).

shrink_all(Tuple, Last, Last) ->
   Elem    = element(Last, Tuple),
   ElemGen = element(Last, TupleGen),
   NewElem = rapperl:shrink(ElemGen, Elem),
   setelement(Last, Tuple, NewElem);
shrink_all(Tuple, Index, Last) ->
   Elem     = element(Index, Tuple),
   ElemGen  = element(Index, TupleGen),
   NewElem  = rapperl:shrink(ElemGen, Elem),
   NewTuple = setelement(Index, Tuple, NewElem),
   shrink_all(NewTuple, Index + 1, Last).
