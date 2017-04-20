% simple server example
-module(simple_example).
-export([start_link/1,funsync/1,funasync/1,stop/0,init/1]).

% basic vsn
start_link(State) ->
  register(?MODULE, spawn_link(?MODULE, init, [State])).

funsync(Arg) ->
  ?MODULE ! {{reqsync, Arg}, self()},
  receive
    Reply -> Reply
  end.

funasync(Arg) ->
  ?MODULE ! {{reqasync, Arg}, self()},
  ok.

stop() ->
  ?MODULE ! {stop, self()},
  ok.

init(State) ->
  loop(State).

loop(State) ->
  receive
    {reqsync, Arg, From} ->
      {Reply, NewState} = dosomething(Arg, State),
      From ! {reply, Reply},
      loop(NewState);
    {stop, From} ->
      From ! stopped,
      ok
  end.

dosomething(Arg, State) ->
  Reply = Arg,
  NewState = State,
  {Reply, NewState}.
