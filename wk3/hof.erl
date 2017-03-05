-module(hof).
-export([add/1,times/1,compose/2,id/1,iterate/1]).
-compile(export_all).

add(X) ->
    fun(Y) -> X+Y end.

times(X) ->
    fun(Y) ->
	     X*Y end.

% Composition
compose(F,G) ->
    fun(X) -> G(F(X)) end.

% Composition is associative – meaning that it doesn’t matter how you bracket a composition of three functions – but it’s not commutative – so that F composed with G is not necessarily the same as G composed with F. Find examples of F and G to show cases when the two compositions are the same, and when they are different.
% 
% Define a function that takes a list of functions and composes them together. Can you use any of the functions in the lists module to help you?
composes([F]) -> compose(F,fun id/1);
composes([F,G]) -> compose(F,G);
composes([Y|Ys]) -> compose(Y,composes(Ys)).

id(X) ->
    X.

% Iteration
% Define a function iterate that takes a number N and returns a function that takes a function and returns that function iterated N times. When N is zero, it should return the identity function (that is, the function that returns its argument unchanged).
% Example: iterate a doubler 3 times:
% Iter=iternate(3),Doubler=fun(X)->X*2 end,IterDbl3Times=Iter(Doubler),16=IterDbl3Times(2).
iterate(0) -> fun id/1;
iterate(N) when N>0 ->
  fun(F) ->
      composes(lists:duplicate(N, F))
  end.

iterate2(0) -> fun id/1;
iterate2(N) when N > 0 ->
    fun(Fn) ->
        lists:foldl(fun compose/2, fun id/1, lists:duplicate(N, Fn))
    end.
      
% Twice
% Using compose or otherwise, define a function twice that applies a function to an argument twice. For example, applying “multiply by 3” twice to the argument 2 gives 18 (as applying it once gives 6 and then applying it again gives 18).
twice(Fun, Arg) -> F2=compose(Fun, Fun), F2(Arg).

% What happens when you apply twice to itself? What happens when you apply the result of that to “multiply by 3” and the result of that to 2?
 
addN(N)->fun(X)->X+N end.
multN(N)->fun(X)->X*N end.
subN(N)->fun(X)->X-N end.
dbl(X)->X+X.

run_tests() ->
  A1=compose(fun addN/1(1), fun multN/1(2)), 8=A1(3),
  A2=compose(fun addN/1(1), compose(fun multN/1(2), fun subN/1(1))), 7=A2(3),
  A3=composes([fun addN/1(1), compose(fun multN/1(2), fun subN/1(1))]), 7=A3(3),
  18=twice(fun multN/1(3), 2),
  Iter1=iterate(1), IterDbl1Times=Iter1(fun dbl/1), 4=IterDbl1Times(2),
  Iter3=iterate(3), IterDbl3Times=Iter3(fun dbl/1), 16=IterDbl3Times(2),
  IIter1=iterate2(1), IIterDbl1Times=IIter1(fun dbl/1), 4=IIterDbl1Times(2),
  IIter3=iterate2(3), IIterDbl3Times=IIter3(fun dbl/1), 16=IIterDbl3Times(2),
  ok.

