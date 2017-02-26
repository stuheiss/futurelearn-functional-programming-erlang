-module('2.7').
-compile(export_all).

area({circle, {_X,_Y}, R}) ->
  math:pi() * R * R;
area({rectangle, {_X,_Y}, H, W}) ->
  H * W;
% Heron's formula to calculate area of triangle
area({triangle, {_X,_Y}, A, B, C}) ->
  S=(A+B+C) / 2,
  math:sqrt(S*(S-A)*(S-B)*(S-C)).

% direct recursion
total_area([]) -> 0;
total_area([X|Xs]) ->
  area(X) + total_area(Xs).

sum([]) -> 0;
sum([X|Xs]) -> X+sum(Xs).

all_areas([]) -> [];
all_areas([X|Xs]) -> [area(X)|all_areas(Xs)].

% generate list of areas and sum the list
total_area2(Xs) ->
  sum(all_areas(Xs)).

circles([]) -> [];
circles([X|Xs]) ->
  case erlang:element(1,X) of
    circle -> [X|circles(Xs)];
    _ -> circles(Xs)
  end.

roundton(D,N) ->
  F=math:pow(10,D),
  round(N * F) / F.

run_tests() ->
  18.57=roundton(2,total_area([{circle,{1,2},2},{rectangle,{5,4},3,2}])),
  18.57=roundton(2,total_area2([{circle,{1,2},2},{rectangle,{5,4},3,2}])),
  [{circle,{1,2},2},{circle,{3,4},5}]=circles([{circle,{1,2},2},{rectangle,{5,4},3,2},{circle,{3,4},5}]),
  ok.
