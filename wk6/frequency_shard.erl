%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency_shard).
-export([allocate/0,deallocate/1,dump/0,w/0,w/1]).
-export([start/0,start/1,stop/0]).
-export([puts/1,try1/0,try2/0,try3/0,try4/0,try5/0]).

puts(Msg) -> io:format("~p puts ~w~n",[self(),Msg]).

try1() ->
  start(roundrobin),
  puts(dump()),
  puts(allocate()),
  puts(dump()),
  stop().

try2() ->
  start(roundrobin),
  puts(dump()),
  {ok,Freq,Worker}=allocate(),
  puts(deallocate(Worker,Freq)),
  puts(dump()),
  stop().

try3() ->
  start(roundrobin),
  puts(dump()),
  {ok,Freq1,_}=allocate(),
  {ok,Freq2,_}=allocate(),
  puts(dump()),
  puts(deallocate(Freq2)),
  puts(deallocate(Freq1)),
  puts(dump()),
  stop().

try4() ->
  io:format("try4: expect to cycle workers round robin~n"),
  start(roundrobin),
  % get a list of workers
  Workers=lists:map(fun({W,_N}) -> W end, get_workers()),
  % allocate a frequency on eqch worker
  lists:map(fun(_) -> allocate() end, Workers),
  % bump the strategy a few times
  lists:map(fun(_) -> puts(strategey()) end, [0,1,2,3,4,5,6,7,8,9]),
  stop().

try5() ->
  io:format("try5: allocate a frequency from each worker~n"),
  % get a list of workers
  Workers=lists:map(fun({W,_N}) -> W end, get_workers()),
  try5(Workers).

% allocate frequencies and deallocate the freq on a selected worker
% expect to see this worker always selected for next op
% do this for each worker
try5([]) -> ok;
try5([Worker|Workers]) ->
  start(max_free_freq),
  L=[allocate(),allocate(),allocate()],
  io:format("try5: free frequencies on ~w~n",[Worker]),
  lists:map(fun({ok,F,W}) -> if Worker /= W -> ok; true -> deallocate(F) end end, L),
  io:format("try5: expect to use ~w next: ~w~n",[Worker,strategey()]),
  io:format("try5: expect to use ~w next: ~w~n",[Worker,strategey()]),
  io:format("try5: expect to use ~w next: ~w~n",[Worker,strategey()]),
  stop(),
  try5(Workers).

%% These are the start functions used to create and
%% initialize the server.

% hard coded list of workers [{Name,Index}]
get_workers() ->
  [{f0,0},{f1,1},{f2,2}]. % try 3 workers
  %[{f0,0},{f1,1}]. % try 2 workers

% hard coded strategey can be roundrobin or max_free_freq
get_strategey_roundrobin() ->
  fun(Args) -> roundrobin(Args) end.

get_strategey_max_free_freq() ->
  fun(Args) -> max_free_freq(Args) end.

% start workers and frontend
% pass the desired strategy as the arguement
% available strategies are max_free_freq and roundrobin
start() -> io:format("try start(max_free_freq) or start(roundrobin)~n").

start(max_free_freq) ->
  start_strategey(get_strategey_max_free_freq());
start(roundrobin) ->
  start_strategey(get_strategey_roundrobin()).

% start workers and frontend with a desired strategy
start_strategey(Strategey) ->
  start_frontend(start_workers(get_workers(), []), Strategey).

% start the fronted
start_frontend(Workers,Strategey) ->
  register(frontend, spawn(fun()->loop_frontend(Workers,[],Strategey) end)).

% start the workers
start_workers([], Acc) -> Acc;
start_workers([{Server,Index}|Servers], Acc) ->
  start_workers(Servers, [start_server(Server,Index)|Acc]).

% stop frontend and workers
stop() ->
  stop_frontend(),
  stop_workers(get_workers()),
  stopped.

stop_workers([]) -> ok;
stop_workers([{Server,_Index}|Servers]) ->
  stop_server(Server),
  stop_workers(Servers).

% start one worker
start_server(Server,N) ->
  register(Server, spawn(fun()->init(N) end)),
  Server.

% start loop with a pool of frequencies on this worker
init(N) ->
  Frequencies = {get_frequencies(N), []},
  loop(Frequencies).

% Hard Coded
% f0: [10..15]
% f1: [20..25]
% f2: [30..35]
% etc.
get_frequencies(N) ->
  Base=10,
  Low=Base+(N*10),
  High=Low+5,
  lists:seq(Low,High).

%% The Main Loop

loop(Frequencies) ->
  %io:format("loop pid ~p fx ~p waiting~n",[self(),Frequencies]),
  receive
    {request, Pid, dump} ->
      Pid ! {reply, Frequencies},
      loop(Frequencies);
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = handle_allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = handle_deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped};
    Msg ->
      io:format("loop unexpected ~p~n",[Msg])
  end.

%% The frontend loop

loop_frontend(Workers,State,Strategey) ->
  {Worker, NewWorkers} = Strategey(Workers),
  %io:format("loop_fe state ~w worker ~w newworkers ~w state ~w waiting~n",[Workers,Worker,NewWorkers,State]),
  receive
    {request, Pid, strategey} ->
      Pid ! {reply, {Worker,NewWorkers}},
      loop_frontend(NewWorkers,State,Strategey);
    {request, Pid, dump} ->
      Pid ! {reply, Workers},
      loop_frontend(NewWorkers,State,Strategey);
    {request, Pid, allocate} ->
      Reply=allocate(Worker), % do rpc to worker
      case Reply of
      %case allocate(Worker) of % do rpc to worker
        {ok,Freq} ->
          Pid ! {reply, {ok,Freq,Worker}},
          loop_frontend(NewWorkers,[{Worker,Freq}|State],Strategey);
        Msg ->
          Pid ! {reply, Msg},
          loop_frontend(Workers,State,Strategey)
      end;
    {request, Pid , {deallocate, Freq}} ->
      case lists:keyfind(Freq,2,State) of
        false ->
          Pid ! {reply, {error, deallocate, no_such_frequency}},
          loop_frontend(Workers,State,Strategey);
        {W,F} ->
          NewState=lists:keydelete(Freq,2,State),
          case deallocate(W,F) of % do rpc to worker
            ok ->
              Pid ! {reply, ok},
              loop_frontend(NewWorkers,NewState,Strategey);
            Msg ->
              Pid ! {reply, Msg},
              loop_frontend(Workers,State,Strategey)
          end
      end;
    {request, Pid, stop} ->
      Pid ! {reply, stopped};
    Msg ->
      io:format("loop_fe unexpected ~p~n",[Msg])
  end.

% round robin strategey
% use the next worker in the list
roundrobin([X|Xs]) ->
  {X,Xs ++ [X]}.

% max free frequencies strategey
% use the worker with the most unused frequencies
max_free_freq(Workers) ->
  {Worker,_NFree}=lists:foldl(fun(X,Acc) ->
     {_WorkerAcc,NFreeAcc}=Acc,
     {XFree,_XAlloc}=dump(X),
     NFreeX=length(XFree),
     if NFreeX > NFreeAcc
       ->
         {X,NFreeX};
       true ->
         Acc
     end
     end, {ok,-1}, Workers),
  {Worker,Workers}.

%% Functional interface

allocate() ->
  frontend ! {request, self(), allocate},
  receive
    {reply, Reply} -> Reply
  end.

deallocate(Freq) ->
  frontend ! {request, self(), {deallocate, Freq}},
  receive
    {reply, Reply} -> Reply
  end.

w() ->
  w([frontend|lists:map(fun({W,_N}) -> W end, get_workers())]).

w(Servers) -> w(Servers, []).
w([], Acc) -> Acc;
w([Server|Servers], Acc) ->
  w(Servers, [{Server,whereis(Server)}|Acc]).

dump() ->
  [dump(frontend)|dump_workers(get_workers(), [])].

dump_workers([], Acc) -> Acc;
dump_workers([{Server,_}|Servers], Acc) ->
  dump_workers(Servers, [{Server,dump(Server)}|Acc]).

dump(Server) ->
  Server ! {request, self(), dump},
  receive
    {reply, Reply} -> Reply
  end.

strategey() ->
  frontend ! {request, self(), strategey},
  receive
    {reply, Reply} -> Reply
  end.

allocate(Worker) ->
  %io:format("~p allocate ~w~n",[self(),Worker]),
  Worker ! {request, self(), allocate},
  receive
    {reply, Reply} -> Reply
  end.

deallocate(Worker,Freq) ->
  %io:format("~p deallocate(~w,~w) send {request,~p,{deallocate,~w}} to ~w~n",[self(),Worker,Freq,self(),Freq,Worker]),
  Worker ! {request, self(), {deallocate, Freq}},
  receive
    {reply, Reply} ->
      Reply;
    Msg ->
      Msg
  end.

stop_server(Server) ->
  Server ! {request, self(), stop},
  receive
    {reply, Reply} -> Reply
  end.

stop_frontend() ->
  stop_server(frontend).


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

handle_allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
handle_allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

handle_deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.
