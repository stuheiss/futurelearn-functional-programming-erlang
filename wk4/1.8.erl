-module('1.8').
-export([sender/0,looper/0,sender2/0,looper2/0,sender3/0,looper3/1,sender4/0,looper4/0]).

sender() ->
  P=spawn(?MODULE,looper,[]),
  P ! {msg, "msg 1"},
  P ! {msg, "msg 2"},
  P ! {msg, "msg 3"},
  P ! {msg, "msg 4"},
  P ! {msg, "msg 5"},
  P ! stop.

% receive all, stop on atom stop, pattern match in receive clause
looper() ->
  %timer:sleep(500),
  receive
    stop ->
      ok;
    X ->
      io:format("got ~p~n", [X]),
      looper()
  end.

sender2() ->
  P=spawn(?MODULE,looper2,[]),
  P ! {msg, "msg 1"},
  P ! {msg, "msg 2"},
  P ! {msg, "msg 3"},
  P ! {msg, "msg 4"},
  P ! {msg, "msg 5"},
  P ! stop.

% receive all, stop on atom stop, pattern match on received value
looper2() ->
  %timer:sleep(500),
  receive
    X ->
      case X of
        stop ->
          ok;
        _ ->
          io:format("got ~p~n", [X]),
          looper2()
      end
  end.

sender3() ->
  P=spawn(?MODULE,looper3,[[first,second,stop,1,2,3,4,5]]),
  P ! {stop, "stop"},
  P ! {second, "second"},
  P ! {first, "first"},
  ok.

% receive in sequence by matching on first elem of argument
looper3([X|Xs]) ->
  receive
    {X, Y} ->
      case X of
        stop ->
          io:format("stopping~n"),
          ok;
        _ ->
          io:format("got ~p ~p~n", [X,Y]),
          looper3(Xs)
      end
  end.

sender4() ->
  P=spawn(?MODULE,looper4,[]),
  P ! {stop, "stop"},
  P ! {second, "second"},
  P ! {first, "first"},
  ok.

% receive in sequence using successive receive blocks
looper4() ->
  receive
    {first, X} ->
      io:format("got ~p ~p~n", [first,X])
  end,
  receive
    {second, Y} ->
      io:format("got ~p ~p~n", [second,Y])
  end,
  receive
    {Atom, Z} ->
      io:format("got ~p ~p~n", [Atom,Z])
  end,
  io:format("stopping~n").

