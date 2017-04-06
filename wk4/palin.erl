-module(palin).
-export([palin/1,nopunct/1,palindrome/1]).
-export([server/1,palindrome_server/1]).
-export([server2/0,palindrome_server2/0]).
-export([server3/0,palindrome_server3/2,palindrome_server4/0]).

% simple single server, spawn with your pid for reply
server(Pid) ->
  spawn(?MODULE, palindrome_server, [Pid]).

palindrome_server(Pid) ->
  receive
    {check, Msg} ->
      Pid ! {result, palindrome(Msg)},
      palindrome_server(Pid);
    _ ->
      stop
  end.

% simple single server, include your pid in the request for reply
server2() ->
  spawn(?MODULE, palindrome_server2, []).

palindrome_server2() ->
  receive
    {check, Pid, Msg} ->
      Pid ! {result, palindrome(Msg)},
      palindrome_server2();
    _ ->
      stop
  end.

% round-robin servers
% start 2 worker processes and 1 dispatcher process
server3() ->
  P1=spawn(?MODULE, palindrome_server4, []),
  io:format("spawned worker ~p~n", [P1]),
  P2=spawn(?MODULE, palindrome_server4, []),
  io:format("spawned worker ~p~n", [P2]),
  P3=spawn(?MODULE, palindrome_server3, [P1,P2]),
  io:format("spawned dispatcher ~p~n", [P3]),
  P3.

% dispatcher
% forward the request to the P1 worker and recurse swapping P1 and P2
% next request will be forwared to the P2 worker
% repeat the cycle alternating workers
palindrome_server3(P1,P2) ->
  receive
    {check, Pid, Msg} ->
      io:format("~p recvd check from ~p on ~p dispatch to ~p~n", [self(),Pid,Msg,P1]),
      P1 ! {check, Pid, Msg},
      palindrome_server3(P2,P1);
    _ ->
      io:format("~p sending stop to ~p and ~p and exiting~n", [self(),P1,P2]),
      P1 ! stop,
      P2 ! stop,
      stop
  end.

% handle request received from dispatcher
palindrome_server4() ->
  receive
    {check, Pid, Msg} ->
      io:format("check from ~p on ~p handled by ~p~n", [Pid, Msg, self()]),
      Pid ! {result, palindrome(Msg)},
      palindrome_server4();
    _ ->
      stop
  end.

% palindrome problem
%
% palindrome("Madam I\'m Adam.") = true

palindrome(Xs) ->
    palin(nocaps(nopunct(Xs))).

nopunct([]) ->
    [];
nopunct([X|Xs]) ->
    case lists:member(X,".,\ ;:\t\n\'\"") of
	true ->
	    nopunct(Xs);
	false ->
	    [ X | nopunct(Xs) ]
    end.

nocaps([]) ->
    [];
nocaps([X|Xs]) ->
    [ nocap(X) | nocaps(Xs) ].

nocap(X) ->
    case $A =< X andalso X =< $Z of
	true ->
	    X+32;
	false ->
	    X
    end.

% literal palindrome

palin(Xs) ->
    Xs == reverse(Xs).

reverse(Xs) ->
    shunt(Xs,[]).

shunt([],Ys) ->
    Ys;
shunt([X|Xs],Ys) ->
    shunt(Xs,[X|Ys]).

 
	


