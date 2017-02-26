-module('1.15').
-export([is_zero/1,xOr0/2,xOr1/2,xOr2/2,xOr3/2,xOr4/2,xOr5/2,maxThree/3,howManyEqual/3]).

is_zero(0) ->
    true;
is_zero(_) ->
    false.

xOr0(true,false) ->
    true;
xOr0(false,true) ->
    true;
xOr0(_,_) ->
    false.

xOr1(X,X) ->
    false;
xOr1(_,_) ->
    true.

xOr2(true,true) ->
    false;
xOr2(false,false) ->
    false;
xOr2(_,_) ->
    true.

xOr3(X,Y) when X == Y ->
    false;
xOr3(_,_) ->
    true.

xOr4(X,Y) when X =/= Y ->
    true;
xOr4(_,_) ->
    false.

xOr5(X,Y) when not X == Y ->
    true;
xOr5(_,_) ->
    false.

maxThree(A,B,C) ->
    max(max(A,B),C).

howManyEqual(A,A,A) -> 3;
howManyEqual(A,A,_) -> 2;
howManyEqual(A,_,A) -> 2;
howManyEqual(_,A,A) -> 2;
howManyEqual(_,_,_) -> 0.
