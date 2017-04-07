%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([init/0,start/0]).
-export([allocate/0,deallocate/1,stop/0]).

start() ->
  %register(?MODULE, spawn(?MODULE, init, [])).
  register(?MODULE, spawn(fun() -> init() end)).

allocate() ->
  frequency ! {request,self(),allocate},
  receive {reply,Reply} -> Reply end.

deallocate(Freq) ->
  frequency ! {request,self(),{deallocate,Freq}},
  receive {reply,Reply} -> Reply end.

stop() ->
  frequency ! {request,self(),stop},
  receive {reply,Reply} -> Reply end.

%% These are the start functions used to create and
%% initialize the server.

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      {NewFrequencies, Reply} = deallocate(Frequencies, Freq, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  case lists:keymember(Pid,2,Allocated) of
    true ->
      {{[Freq|Free], Allocated}, {error, has_frequency}};
    false ->
      {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}
  end.

deallocate({Free, Allocated}, Freq, Pid) ->
  case lists:member({Freq,Pid},Allocated) of
    true ->
      NewAllocated=lists:keydelete(Freq, 1, Allocated),
      {{[Freq|Free],  NewAllocated}, ok};
    false ->
      {{Free, Allocated}, {error, not_allocated}}
  end.
