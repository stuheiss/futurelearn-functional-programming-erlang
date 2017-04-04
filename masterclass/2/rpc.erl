-module(rpc).
-export([rpc/2]).

% client
rpc(Pid, Request) ->
  Tag = erlang:make_ref(),
  Pid ! {self(), Tag, Request},
  receive
    {Tag, Response} ->
      Response
  end.
