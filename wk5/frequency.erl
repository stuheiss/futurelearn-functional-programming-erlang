%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).
-export([start_sup/0,stop_sup/0,kill_sup/0,kill_freq/0,kill_all/0]).
-export([spawn_client/1,client/1]).
-export([w/0,dump/0]).

% spawn a client model process
spawn_client(Sleep) ->
  spawn(fun()->client(Sleep) end).

% simple client model
client(Sleep) ->
  {ok,Freq} = allocate(),
  io:format("~p client start call on ~w for ~wms~n",[self(),Freq,Sleep]),
  timer:sleep(Sleep),
  io:format("~p client end call on ~w after ~wms~n",[self(),Freq,Sleep]),
  deallocate(Freq).

%% These are the start functions used to create and
%% initialize the server.

% tell status of supervisor and frequency servers
w() ->
  io:format("supervisor ~p, frequency ~p~n",[whereis(supervisor),whereis(frequency)]).

% start supervisor
start_sup() ->
  register(supervisor, spawn(fun()->supervisor() end)).

% stop supervisor
stop_sup() ->
  kill_sup().

% stop supervisor and frequency servers
kill_all() ->
  kill_freq(),
  kill_sup().

% kill supervisor
kill_sup() ->
  case whereis(supervisor) of
    undefined -> ok;
    PidS -> exit(PidS, kill)
  end.

% kill frequency server
kill_freq() ->
  case whereis(frequency) of
    undefined -> ok;
    PidF -> exit(PidF, kill)
  end.

% the supervisor
supervisor() ->
  process_flag(trap_exit, true),    %%% Set trap
  register(frequency, spawn_link(fun()->init() end)),
  io:format("~p supervisor: start frequency server ~p~n",[self(),whereis(frequency)]),
  sup_loop().

% the supervisor loop
sup_loop() ->
  io:format("~p supervisor waiting...~n",[self()]),
  receive
    {'EXIT',Pid,Reason} ->
      io:format("~p supervisor: frequency server died with reason ~p, restarting ~p~n",[self(),Reason,Pid]),
      register(frequency, spawn_link(fun()->init() end)),
      sup_loop();
    Msg ->
      io:format("~p supervisor: unexpected ~p~n",[self(),Msg]),
      sup_loop()
  end.

% start freqency server
start() ->
    register(frequency,
	     spawn(frequency, init, [])).

% start freqency looper
init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, Tag, dump} ->
      Pid ! {reply, Tag, {ok, Frequencies}},
      loop(Frequencies);
    {request, Pid, Tag, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Tag, Reply},
      loop(NewFrequencies);
    {request, Pid, Tag, {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, Tag, ok},
      loop(NewFrequencies);
    {request, Pid, Tag, stop} ->
      Pid ! {reply, Tag, stopped};
    {'EXIT', Pid, _Reason} ->                   %%% CLAUSE ADDED
      NewFrequencies = exited(Frequencies, Pid),
      loop(NewFrequencies)
  end.

%% Functional interface

dump() ->
  {ok,Frequencies}=frequency(dump),
  io:format("~p~n",[Frequencies]).

allocate() ->
  frequency(allocate).

deallocate(Freq) ->
  frequency({deallocate, Freq}).

stop() ->
  frequency(stop).

% rpc helper
frequency(Msg) ->
  Tag = make_ref(),
  frequency ! {request, self(), Tag, Msg},
  receive
    {reply, Tag, Reply} -> Reply
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),                                               %%% ADDED
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  {value,{Freq,Pid}} = lists:keysearch(Freq,1,Allocated),  %%% ADDED
  unlink(Pid),                                             %%% ADDED
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

exited({Free, Allocated}, Pid) ->                %%% FUNCTION ADDED
  io:format("~p exited free=~p, allocated=~p}~n",[Pid,Free,Allocated]),
    case lists:keysearch(Pid,2,Allocated) of
      {value,{Freq,Pid}} ->
        NewAllocated = lists:keydelete(Freq,1,Allocated),
        {[Freq|Free],NewAllocated};
      false ->
        {Free,Allocated}
    end.

