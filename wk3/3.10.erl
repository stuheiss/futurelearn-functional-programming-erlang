-module('3.10').
-compile(export_all).

% partially applied functions
add(X) ->
  fun(Y) -> X+Y end.

addOneToAll(Xs) ->
  lists:map(add(1), Xs).

addToAll(N,Xs) ->
  lists:map(add(N), Xs).

% composition
compose(F,G) ->
  fun(X) -> G(F(X)) end.

run_tests() ->
  [2,3,4,5,6]=addOneToAll([1,2,3,4,5]),
  [2,3,4,5,6]=addToAll(1,[1,2,3,4,5]),
  [3,4,5,6,7]=addToAll(2,[1,2,3,4,5]),
  ok.
