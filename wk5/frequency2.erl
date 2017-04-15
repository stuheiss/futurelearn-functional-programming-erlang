%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency2).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).
-export([loop/1]).
-export([inject/1]).
-export([dump/0]).

%% Hot code loading is working!!!
%% 1. change loop(...) to frequency2:loop(...) in recursive calls
%% 2. add new dump/0 inject/1 functions
%%
%% To load new code, do:
%% code:soft_purge(frequency2)
%% code:load_file(frequency2)
%% force recursion to load new loop/0
%% ex: frequency2:allocate() will do the job
%% Use new features!

%% These are the start functions used to create and
%% initialize the server.

start() ->
  register(frequency2, spawn(frequency2, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid , {inject, MoreFrequencies}} ->
      NewFrequencies = inject(Frequencies, MoreFrequencies),
      Pid ! {reply, ok},
      frequency2:loop(NewFrequencies);
    {request, Pid, dump} ->
      Pid ! {reply, Frequencies},
      frequency2:loop(Frequencies);
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      frequency2:loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      frequency2:loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% Functional interface

inject(Frequencies) ->
  frequency2 ! {request, self(), {inject, Frequencies}},
  receive
    {reply, Reply} -> Reply
  end.

dump() ->
  frequency2 ! {request, self(), dump},
  receive
    {reply, Reply} ->
      io:format("~w~n",[Reply]),
      ok
  end.

allocate() ->
  frequency2 ! {request, self(), allocate},
  receive
    {reply, Reply} -> Reply
  end.

deallocate(Freq) ->
  frequency2 ! {request, self(), {deallocate, Freq}},
  receive
    {reply, Reply} -> Reply
  end.

stop() ->
  frequency2 ! {request, self(), stop},
  receive
    {reply, Reply} -> Reply
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

inject({Free, Allocated}, MoreFrequencies) ->
  {Free ++ MoreFrequencies,  Allocated}.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.
