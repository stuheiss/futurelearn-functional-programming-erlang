-module('1.18').
-export([fac/1,fib/1,pieces/1,pieces3d/1]).

fac(0) ->
    1;
fac(N) when N>0 ->
    fac(N-1)*N.

% 0,1,1,2,3,5
fib(0) ->
    0;
fib(1) ->
    1;
fib(2) ->
    1;
fib(N) when N>0 ->
    fib(N-1) + fib(N-2).

% Define a function pieces so that pieces(N) tells you the maximum number
% of pieces into which you can cut a piece of paper with N cuts.
pieces(0) ->
    1;
pieces(N) when N>0 ->
    N+pieces(N-1).

pieces3d(0) ->
    1;
pieces3d(N) when N>0 ->
    pieces3d(N-1) + pieces(N-1).
