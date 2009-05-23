-module(rapperl_tuple, [TupleGen]).

-export([value/0]).

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
