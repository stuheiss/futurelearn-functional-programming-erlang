-module(counter0).
-export([start/0,tick/1,read/0,loop/1]).

%% client
start() ->
  register(counter0, spawn(counter0, loop, [0])).

tick(N) ->
  rpc({tick, N}).

read() ->
  rpc(read).

rpc(Query) ->
  Tag = make_ref(),
  counter0 ! {self(), Tag, Query},
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
