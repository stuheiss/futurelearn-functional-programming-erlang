-module(counter).
-export([start/1,tick/2,read/1,loop/1]).

%% client
start(Name) ->
  register(Name, spawn(?MODULE, loop, [0])).

tick(Name,N) ->
  rpc(Name, {tick, N}).

read(Name) ->
  rpc(Name, read).

rpc(Process, Query) ->
  Tag = make_ref(),
  Process ! {self(), Tag, Query},
  receive
    {Tag, Reply} ->
      Reply
  end.

%% server
loop(State) ->
  receive
    {From, Tag, {tick, N}} ->
      From ! {Tag, ack},
      loop(State + N);
    {From, Tag, read} ->
      From ! {Tag, State},
      loop(State)
  end.
