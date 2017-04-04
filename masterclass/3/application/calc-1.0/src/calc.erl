% calc client using gen_server behaviour
-module(calc).
-behaviour(gen_server).

-export([start/1, stop/0, eval/1, print/1]).
-export([handle_call/3, handle_cast/2, init/1, terminate/2]).

start(Env) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Env, []).

init(Env) ->
  io:format("Starting...~n"),
  {ok, Env}.

terminate(_Reason, _Env) ->
  io:format("Terminating...~n").

% client initiated async call
stop() ->
  gen_server:cast(?MODULE, stop).

print(Expr) ->
  gen_server:cast(?MODULE, {print, Expr}).

% client initiated synchronous call
eval(Expr) ->
  gen_server:call(?MODULE, {eval, Expr}).

% server call to client function in response to client synchronous request
handle_call({eval, Expr}, _From, Env) ->
  %{reply, expr:eval(Env, Expr), Env}.
  {reply, rpn:rpn(Expr), Env}.

% server async call to client function in response to client async request
handle_cast({print, Expr}, Env) ->
  Str=rpn:print(Expr),
  io:format("~s", [Str]),
  {noreply, Env};
handle_cast(stop, Env) ->
  {stop, normal, Env}.
