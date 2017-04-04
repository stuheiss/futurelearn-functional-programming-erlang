% calc client/server all-in-one
-module(calc0).
-export([start/1, stop/0, eval/1]).
-export([init/1]).

start(Env) ->
  register(calc0, spawn(calc0, init, [Env])).

stop() ->
  calc0 ! stop.

eval(Expr) ->
  calc0 ! {request, self(), {eval, Expr}},
  receive
    {reply, Reply} ->
      Reply
  end.

init(Env) ->
  io:format("Starting...~n"),
  loop(Env).

loop(Env) ->
  receive
    {request, From, {eval, Expr}} ->
      %From ! {reply, expr:eval(Env, Expr)},
      From ! {reply, rpn:rpn(Expr)},
      loop(Env);
    stop ->
      io:format("Terminating...~n")
  end.
