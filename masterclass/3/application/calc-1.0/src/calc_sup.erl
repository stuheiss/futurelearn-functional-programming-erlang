% calc supervisor behaviour
-module(calc_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Env) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Env).

init(Env) ->
  io:format("Starting supervisor...~n"),
  SupFlags={one_for_one, 10, 3600},
  ChildSpec={calc, {calc, start, [Env]},
             permanent, 2000, worker, [calc]},
  {ok, {SupFlags, [ChildSpec]}}.

