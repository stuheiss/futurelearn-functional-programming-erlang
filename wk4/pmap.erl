-module(pmap).
-compile(export_all).

% typical pmap(F,List) maps F over List
pmap(F,L) ->
  Do=fun(Fun,Elem) ->
         Parent=self(),
         Ref=erlang:make_ref(),
         Child=spawn(fun() ->
                         Parent ! {self(), Ref, Fun(Elem)}
                     end),
         {Child,Ref}
     end,
  PidRefList = [Do(F,E) || E <- L],
  [receive {Pid,Ref,Val} -> Val end || {Pid,Ref} <- PidRefList].

% usage: Result=pmap([F1,F2,Fn]).
% usage: Result=pmap([fun1/0,fun2/0,fun3/0]).
% usage: Result=pmap([fun()->1 end,fun()->2 end,fun()->3 end]).
% usage: Result=pmap([fun()->X end || X <- [1,2,3,4,5]]).
% usage: Result=pmap([fun()->OtherFun(X) end || X <- [1,2,3,4,5]]).
% where OtherFun=fun X(N)->if N < 3 -> N; true -> X(N-1)+X(N-2) end end.
% (parallel fibs!)

% pmap in one function
% add refs to messages for safetx
pmap2(L) ->
  Do=fun(F) ->
         Parent=self(),
         Ref=erlang:make_ref(),
         Child=spawn(fun() ->
                         Parent ! {self(), Ref, F()}
                     end),
         {Child,Ref}
     end,
  PidRefList = [Do(F) || F <- L],
  [receive {Pid,Ref,Val} -> Val end || {Pid,Ref} <- PidRefList].

fib(N) ->
  if N < 3 -> N; true -> fib(N-1)+fib(N-2) end.

try1() ->
  X=35,
  pmap2([fun()->{Y,fib(Y)} end || Y <- [X,X,X,X,X,X,X,X,X,X]]).

try3() ->
  pmap2([fun()->{X,fib(X)} end || X <- [35,2,3,35,35,35,35,35,35,35,1]]).

t(F) ->
  {_,Ssec,Sms}=erlang:timestamp(),
  _Result=F(),
  {_,Esec,Ems}=erlang:timestamp(),
  io:format("begin: ~p ~p, end ~p ~p~n",[Ssec,Sms,Esec,Ems]),
  Elapsed=( (1000000* (Esec-Ssec)) + (Ems-Sms)) / 1000000,
  io:format("elapsed ~p~n", [Elapsed]),
  Elapsed.


