-module('2.11').
-compile(export_all).
% types

-spec take(integer(), [T]) -> [T].
take(0,_) -> [];
take(_,[]) -> [];
take(N,[X|Xs]) when N>0 -> [X|take(N-1,Xs)].


run_tests() ->
  [] = take(0, "hello"),
  "hell" = take(4, "hello"),
  "hello" = take(5, "hello"),
  "hello" = take(9, "hello"),
  ok.
