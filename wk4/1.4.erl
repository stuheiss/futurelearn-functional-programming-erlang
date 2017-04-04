% Erlang concurrency in a nutshell
% Erlang concurrency primatives:
% spawn   - create a process
% !       - send a message
% receive - receive a message
% self()  - get a process id (pid)

% spawn(Mod, Fun, Args)
% ex: Proc=spawn(foo, bar, [])
% 
% send a message to Pid
% return value of send is the message
% return value is typically ignored
% sending a message to a non-existent Pid is a no-op, not an error
% ex: Pid ! message

-module('1.4').
-export([bar/0,bar/1,baz/0,bazz/0]).

bar() ->
  timer:sleep(500),
  io:format("bar started~n"),
  io:format("bar working~n"),
  io:format("bar finished~n").

bar(Pid) ->
  Pid ! "bar started~n",
  Pid ! "bar working~n",
  Pid ! "bar finished~n".

baz() ->
  receive
    Msg ->
      io:format("got: ~p~n", [Msg])
  end,
  baz().

bazz() ->
  receive
    stop ->
      io:format("stopped~n");
    Msg ->
      io:format("got: ~p~n", [Msg]),
      bazz()
  end.

