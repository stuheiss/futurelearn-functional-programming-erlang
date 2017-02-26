-module('2.14').
-compile(export_all).

% keep first
% [2,4,1,3,3,1] -> [2,4,1,3]
% use removeAll
nub([]) -> [];
nub([X|Xs]) ->
  [X|nub(removeAll(X,Xs))].

% use lists:filter
nubf([]) -> [];
nubf([X|Xs]) ->
  [X|nub(lists:filter(fun(Y)->X=/=Y end,Xs))].

% recursive pattern match
removeAll(_,[]) -> [];
removeAll(X,[X|Xs]) -> removeAll(X,Xs);
removeAll(X,[Y|Xs]) -> [Y|removeAll(X,Xs)].

% direct recursive loop
% naive, calls lists:member and lists:delete repeatedly
removeAll2(_,[]) -> [];
removeAll2(X,Xs) ->
  case lists:member(X,Xs) of
    true -> removeAll2(X,lists:delete(X,Xs));
    false -> Xs
  end.

% keep last
% [2,4,1,3,3,1] -> [2,4,3,1]
bun([]) -> [];
bun([X|Xs]) ->
  case lists:member(X,Xs) of
    true -> bun(Xs);
    false -> [X|bun(Xs)]
  end.

member(_,[]) -> false;
member(X,[X|_]) -> true;
member(X,[_|Xs]) -> member(X,Xs).

run_tests() ->
  [2,4,1,3]=nub([2,4,1,3,3,1]),
  [2,4,1,3]=nubf([2,4,1,3,3,1]),
  [2,4,3,1]=bun([2,4,1,3,3,1]),
  true=member(3,[1,2,3,4,5]),
  false=member(6,[1,2,3,4,5]),
  [1,2,3]=removeAll(4,[1,2,3,4]),
  [1,2,3,4]=removeAll(5,[1,2,3,4]),
  [1,2,3]=removeAll2(4,[1,2,3,4]),
  [1,2,3,4]=removeAll2(5,[1,2,3,4]),
  ok.
