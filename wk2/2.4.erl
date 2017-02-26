-module('2.4').
%-export([]).
-compile(export_all).

sum([]) -> 0;
sum([X|Xs]) -> X+sum(Xs).

sumt([],A) -> A;
sumt([X|Xs],A) -> sumt(Xs,X+A).
sumt(Xs) -> sumt(Xs,0).

% direct recursive
% assume product of empty list is 1
product([]) -> 1;
product([X]) -> X;
product([X|Xs]) -> X*product(Xs).

% tail recursive version
productT(Xs) -> productT(Xs,1).
productT([],A) -> A;
productT([X|Xs],A) -> productT(Xs,X*A).

% direct recursive
maximum([X]) -> X;
maximum([X|Xs]) -> max(X,maximum(Xs)).

% tail recursive version
maximumT(Xs) when Xs =/= [] -> maximumT(Xs,0).
maximumT([],A) -> A;
maximumT([X|Xs],A) -> maximumT(Xs,max(X,A)).


run_tests() ->
	0=sum([]),
	15=sum([1,2,3,4,5]),
	0=sumt([]),
	15=sumt([1,2,3,4,5]),
	1=product([]),
	60=product([3,4,5]),
	1=productT([]),
	60=productT([3,4,5]),
	5=maximum([1,2,3,4,5]),
	5=maximumT([1,2,3,4,5]),
	ok.
