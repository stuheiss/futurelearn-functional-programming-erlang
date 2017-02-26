-module('1.23').
-export([area/1,fib/1,fib1/1,fib2/1]).

% pattern matching
% distinguish different cases
% pull components out of complex data structures

% simple recursion
fib(0) -> 0;
fib(1) -> 1;
fib(N) when N>1 -> fib(N-1) + fib(N-2).

% tail recursion using vars as accumulators
fib1(0,P,_C) -> P;
fib1(N,P,C) -> fib1(N-1,C,P+C).
fib1(N) -> fib1(N,0,1).

% tail recursion using vars as result
fibP(0) -> {0,1};
fibP(N) ->
  {P,C} = fibP(N-1),
  {C,C+P}.
fib2(N) ->
  {P,_C} = fibP(N),
  P.

% geometric shapes
% {circle, {X,Y}, R}
% {rectangle, {X,Y}, H, W}
area({circle, {_X,_Y}, R}) ->
  math:pi() * R * R;
area({rectangle, {_X,_Y}, H, W}) ->
  H * W.
