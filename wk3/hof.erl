-module(hof).
-compile(export_all).

map(_F, []) -> [];
map(F, [X|Xs]) -> [F(X) | map(F, Xs)].

filter(_P, []) -> [];
filter(P, [X|Xs]) ->
  case P(X) of
    true -> [X | filter(P, Xs)];
    false -> filter(P, Xs)
  end.

reduce(_Combine, Start, []) -> Start;
reduce(Combine, Start, [X | Xs]) ->
  Combine(X, reduce(Combine, Start, Xs)).

% doubleAll([]) -> [];
% doubleAll([X|Xs]) ->
%     [ 2*X | doubleAll(Xs) ].
doubleAll(Xs) -> lists:map(fun(X) -> 2*X end, Xs).

% evens([]) -> [];
% evens([X|Xs]) when X rem 2 == 0 ->
%     [X | evens(Xs) ];
% evens([_|Xs]) ->
%     evens(Xs).
evens(Xs) -> lists:filter(fun(X) -> X rem 2 == 0 end, Xs).

% product([]) -> 1;
% product([X|Xs]) -> X * product(Xs).
product(Xs) -> lists:foldr(fun(X,Y) -> X*Y end, 1, Xs).

% zip([1,3,5,7], [2,4]) = [ {1,2}, {3,4} ]
zip(Xs, Ys) -> zip(Xs, Ys, []).
zip([], _, Acc) -> Acc;
zip(_, [], Acc) -> Acc;
zip([X|Xs], [Y|Ys], Acc) -> zip(Xs, Ys, Acc ++ [{X, Y}]).

% zip_with(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]) = [ 3, 7 ]
zip_with(_F, [], _) -> [];
zip_with(_F, _, []) -> [];
zip_with(F, [X|Xs], [Y|Ys]) -> [F(X,Y) | zip_with(F, Xs, Ys)].

% Re-define the function zip_with/3 using zip and lists:map.
zip_with_a(F,Xs,Ys) -> lists:map(fun({X,Y})->F(X,Y) end, zip(Xs,Ys)).

% Re-define zip/2 using zip_with/3.
zip_a(Xs,Ys) -> zip_with(fun(X,Y) -> {X,Y} end, Xs, Ys).

run_tests() ->
  [ 3, 7 ] = zip_with_a(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]),
  [ 3, 7 ] = zip_with(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]),
  [ {1,2}, {3,4} ] = zip_a([1,3,5,7], [2,4]),
  [ {1,2}, {3,4} ] = zip([1,3,5,7], [2,4]),
  ok.
