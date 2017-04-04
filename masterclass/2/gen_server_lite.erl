% refactor concurrent counter - concurrent part
-module(gen_server_lite).
-export([start/2,loop/2,rpc/2]).

start(Mod,State) ->
  register(Mod,spawn(gen_server_lite,loop,[Mod,State])).

loop(Mod,State) ->
  receive
    {From,Tag,stop} ->
      From ! {Tag,ok};
    {From,Tag,Query} ->
      {Reply,State1} = Mod:handle(Query,State),
      From ! {Tag,Reply},
      loop(Mod,State1)
  end.

rpc(Pid,Query) ->
  Tag = make_ref(),
  Pid ! {self(), Tag, Query},
  receive
    {Tag,Reply} ->
      Reply
  end.
