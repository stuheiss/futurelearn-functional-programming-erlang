% very simple concurrent program implements a counter
% counter process registered as counter0
% initial counter state is 0
% read() returns current state
% tick(N) increments current state by N
-module(counter0).
-export([start/0,loop/1,tick/1,read/0]).

start() ->
  register(counter0,spawn(counter0,loop,[0])).

tick(N) -> rpc({tick,N}).
read() -> rpc(read).

loop(State) ->
  receive
    {From,Tag,{tick,N}} ->
      From ! {Tag,ack},
      loop(State+N);
    {From,Tag,read} ->
      From ! {Tag,State},
      loop(State)
  end.

rpc(Query) ->
  Tag = make_ref(),
  counter0 ! {self(), Tag, Query},
  receive
    {Tag,Reply} ->
      Reply
  end.
