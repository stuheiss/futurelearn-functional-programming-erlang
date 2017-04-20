-module(counter_server).
-export([start/3,rpc/2,loop/3]).

%% generic server
start(Mod, Handle, State) ->
  register(Mod, spawn(?MODULE, loop, [Mod, Handle, State])).

rpc(Process, Query) ->
  Tag = make_ref(),
  Process ! {self(), Tag, Query},
  receive
    {Tag, Reply} ->
      Reply
  end.

loop(Mod, Handle, State) ->
  receive
    {From, Tag, Query} ->
      {Reply, State1} = Handle:handle(Query, State),
      From ! {Tag, Reply},
      loop(Mod, Handle, State1)
  end.
