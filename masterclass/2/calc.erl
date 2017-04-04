-module(calc).
-export([start/0,init/0,loop/0,stop/0,execute/1]).

start() -> spawn(calc, init, []).

init() ->
  io:format("Starting...~n"),
  register(calc,self()),
  loop().

loop() ->
  receive
    {request,From,Expr} ->
      From ! {reply, rpn:rpn(Expr)},
      loop();
    stop ->
      io:format("Stopping...~n")
  end.

stop() -> calc ! stop.

execute(X) ->
  calc ! {request, self(), X},
  receive
    {reply, Reply} ->
      Reply
  end.
