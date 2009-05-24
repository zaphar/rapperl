-module(rapperl_tuple, [TupleGen]).
-export([value/0
        ,shrink_strategies/0
        ,shrinks_with/2
        ,shrink/2]).

value() ->
   gen_val(TupleGen).

gen_val({}) ->
   {};
gen_val({G}) ->
   {rapperl:pop(G)};
gen_val({G0, G1}) ->
   {rapperl:pop(G0), rapperl:pop(G1)};
gen_val({G0, G1, G2}) ->
   {rapperl:pop(G0), rapperl:pop(G1), rapperl:pop(G2)};
gen_val({G0, G1, G2, G3}) ->
   {rapperl:pop(G0), rapperl:pop(G1), rapperl:pop(G2), rapperl:pop(G3)};
gen_val({G0, G1, G2, G3, G4}) ->
   {rapperl:pop(G0), rapperl:pop(G1), rapperl:pop(G2), rapperl:pop(G3), rapperl:pop(G4)}.

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
