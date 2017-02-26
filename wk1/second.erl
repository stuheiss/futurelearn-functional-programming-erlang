-module(second).
-export([hypotenuse/2,perimeter/2,area/2]).

hypotenuse(X,Y) ->
  math:sqrt(first:square(X) + first:square(Y)).

perimeter(X,Y) ->
  X + Y + hypotenuse(X,Y).

area(X,Y) ->
  first:area(X, Y, hypotenuse(X,Y)).

