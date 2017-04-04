% calc server
-module(server).
-export([start/2, stop/1, request/2]).
-export([init/2]).

start(Name, Args) ->
  register(Name, spawn(?MODULE, init, [Name, Args])).

init(Name, Args) ->
  LoopData = Name:init(Args),
  loop(Name, LoopData).

stop(Name) ->
  Name ! stop.

request(Name, Msg) ->
  %Ref = make_ref(),
  Ref = erlang:monitor(process, whereis(Name)),
  Name ! {request, self(), Ref, Msg},
  receive
    {reply, Ref, Reply} ->
      erlang:demonitor(Ref, [flush]),
      Reply;
    {'DOWN', Ref, Type, Pid2, Reason} ->
      io:format("DOWN: Ref=~p Type=~p Pid2=~p Reason=~p~n", [Ref, Type, Pid2, Reason]),
      error(noproc)
  end.

loop(Name, LoopData) ->
  receive
    {request, From, Ref, Msg} ->
      {Reply, NewLoopData} = Name:handle(Msg, LoopData),
      From ! {reply, Ref, Reply},
      loop(Name, NewLoopData);
    stop ->
      Name:terminate(LoopData)
  after
    60000 ->
      io:format("Timeout (60 seconds)!~n"),
      %loop(Name, LoopData)
      Name:terminate(LoopData)
  end.

