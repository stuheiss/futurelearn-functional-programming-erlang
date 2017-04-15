%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([allocate/0,deallocate/1,stop/0]).
-export([init/0]).
-export([start_sup/0,stop_sup/0,kill_sup/0,kill_freq/0,kill_all/0]).
-export([spawn_client/1,client/1]).
-export([w/0,dump/0]).
-export([test1/0,test2/0]).

% test 2 calls with duration 5 seconds
%  - start the supervisor
%  - show supervisor and frequency server to be running
%  - dump the {Free,Allocated} state
%  - start 2 concurrent calls
%  - dump the state again showing 2 frequencies as allocated
%  - wait for both calls to finish, see that they both complete
%  - show supervisor and frequency server to be running
%  - dump the {Free,Allocated} state
%  - kill the supervisor and frequency server
test1() ->
  start_sup(),
  timer:sleep(500),
  w(),
  dump(),
  spawn_client(5000),
  spawn_client(5000),
  timer:sleep(500),
  dump(),
  timer:sleep(6000),
  w(),
  dump(),
  stop_sup(),
  ok.

% test 2 calls with duration 5 seconds and kill the frequency server after one second
% see how the frequency server is restarted by the supervisor
% note that the calls are dropped when the frequency server is killed
% see that the calls dont't complete since the clients were killed when the frequency server died
test2() ->
  start_sup(),
  timer:sleep(500),
  w(),
  dump(),
  spawn_client(5000),
  spawn_client(5000),
  timer:sleep(500),
  dump(),
  timer:sleep(1000),
  kill_freq(),
  timer:sleep(1000),
  w(),
  dump(),
  stop_sup(),
  ok.

% spawn a client model process
spawn_client(Sleep) ->
  spawn(fun()->client(Sleep) end).

% simple client model
% simulate a call for some duration
%  - allocate a frequency
%  - sleep for duration
%  - deallocate the frequency
client(Sleep) ->
  {ok,Freq} = allocate(),
  io:format("~p client start call on ~w for ~wms~n",[self(),Freq,Sleep]),
  timer:sleep(Sleep),
  io:format("~p client end call on ~w after ~wms~n",[self(),Freq,Sleep]),
  deallocate(Freq).

%% These are the start functions used to create and
%% initialize the server.

% start and register the supervisor
start_sup() ->
  register(supervisor, spawn(fun()->supervisor() end)).

% the supervisor
%  - trap exit
%  - start frequency server
%  - start supervisor loop
supervisor() ->
  process_flag(trap_exit, true),    %%% Set trap
  case start_freq() of
    ok ->
      sup_loop();
    Msg ->
      io:format("~p supervisor: start frequency server unexpected ~p",[self(),Msg]),
      error
  end.

% stop supervisor
stop_sup() ->
  kill_sup().

% kill supervisor and frequency servers
kill_all() ->
  kill_freq(),
  kill_sup().

% kill supervisor if running
kill_sup() ->
  case whereis(supervisor) of
    undefined -> ok;
    PidS -> exit(PidS, kill)
  end.

% kill frequency server if running
kill_freq() ->
  case whereis(frequency) of
    undefined -> ok;
    PidF -> exit(PidF, kill)
  end.

% start and register the frequency server
% called by the supervisor
%  - spawn_link a process so supervisor will be notified when process terminates
%  - spawned process will be the frequency server
start_freq() ->
  case register(frequency, spawn_link(fun()->init() end)) of
    true ->
      io:format("~p start frequency server ~p~n",[self(),whereis(frequency)]);
    Msg ->
      io:format("~p failed to start frequency server ~p~n",[self(),Msg]),
      error
  end.

%% start freqency looper with initial pool of free frequencies
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
      loop(NewFrequencies);
    Msg ->
      io:format("~p loop: unexpected ~p~n",[self(),Msg])
  end.

%% The supervisor loop
sup_loop() ->
  receive
    {'EXIT',Pid,Reason} ->
      io:format("~p supervisor: frequency server died with reason ~p, restarting ~p~n",[self(),Reason,Pid]),
      case start_freq() of
        ok ->
          sup_loop();
        _ ->
          error
      end;
    Msg ->
      io:format("~p supervisor: unexpected ~p~n",[self(),Msg]),
      sup_loop()
  end.

%% Functional interface

% show PIDs of supervisor and frequency servers
w() ->
  io:format("supervisor ~p, frequency ~p~n",[whereis(supervisor),whereis(frequency)]).

% dump the frequency server state
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

