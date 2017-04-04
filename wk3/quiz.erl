-module(quiz).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

nth(0,[X|_]) -> X;
nth(N,[_|Xs]) -> nth(N-1,Xs).

t0_test() ->
  ?assertEqual(42, nth(0,[42,1,2,3])).
t1_test() ->
  ?assertEqual(1, nth(1,[42,1,2,3])).
t2_test() ->
  ?assertEqual(2, nth(2,[42,1,2,3])).
t3_test() ->
  ?assertEqual(3, nth(3,[42,1,2,3])).
t4_test() ->
  ?assertEqual(fun nth/2, fun nth/2).
