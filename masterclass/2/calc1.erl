-module(calc1).
-export([start/0,stop/0,execute/1,handle/1]).
-import(gen_server_lite,[start/2,rpc/2]).

start() -> start(calc1, []).

stop() -> rpc(calc1, stop).
execute(X) -> rpc(calc1, {execute,X}).

handle({execute,Expr}) ->
  io:format("handle ~p~n",[Expr]),
  %R=rpn:rpn(Expr),
  R=Expr,
  {R,R}.
