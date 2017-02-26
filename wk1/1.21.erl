-module('1.21').
-export([fac/1,loop/1,fib/1,perfect/1]).

fac(N) ->
  fac(N,1).

fac(0,P) ->
  P;
fac(N,P) when N>0 ->
  fac(N-1,N*P).

loop(N) when N>0 ->
  io:format("~p~n", [N]),
  loop(N-1);
loop(_) ->
  io:format("Bye~n").

% 0, 1, 1, 2, 3, 5,
fib(N) ->
  fib(N,0,1).

fib(0,A,_) ->
  A;
fib(N,A,B) when N>0 ->
  fib(N-1,B,A+B).

% A positive integer is perfect when it is the sum of its divisors,
% e.g. 6=1+2+3, 28=1+2+4+7+14.
perfect(N) when N>0 ->
  perfect(N,1,0).
perfect(N,N,S) ->
  N == S;
%perfect(N,I,S) ->
%  case N rem I == 0 of
%    true ->
%      perfect(N,I+1,S+I);
%    false ->
%      perfect(N,I+1,S)
%  end.
perfect(N,I,S) ->
   perfect(N,I+1,
           case N rem I == 0 of
             true -> S+I;
             false -> S
           end
          ).

% perfect(N) ->
%   N == suml(factors(N)).

% 1=>[1], 2=[2,1], 3=>[3,1], 4=>[2,1], 5=>[5,1], 6=>[3,2,1], 7=>[7,1]...
% is_factor(X,Y) ->
%   (Y rem X) == 0.

% factors(N,I,F) when I>=N ->
%   F;
% factors(N,I,F) when I<N ->
%   case is_factor(I,N) of
%     true -> factors(N,I+1,[I]++F);
%     false -> factors(N,I+1,F)
%   end.

% factors(N) when N>0 ->
%   factors(N,1,[]).

% suml([]) ->
%   0;
% suml([X|Xs]) ->
%   X + suml(Xs).

