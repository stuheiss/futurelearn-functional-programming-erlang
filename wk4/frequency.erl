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
-export([clear/0]).

% uncomment next line to include unit tests
-define(Testing, true).

-ifdef(Testing).
-include_lib("eunit/include/eunit.hrl").
-endif.

% uncomment next line to simulate overload and message timeouts
%-define(SimulateOverload, true).

-ifdef(SimulateOverload).
-define(Overload, timer:sleep(2000)).
-else.
-define(Overload, true).
-endif.

% see unit tests at bottom of file:
% one_client_test() - allocate/deallocate one frequency
% multiple_client_test() - allocate/deallocate four frequencies in separate processes
% Expect unit tests to fail if simulating high load and timeouts

% Note: a process can only allocate one frequency at a time
% Note: a process can only deallocate the frequency currently allocated
% Note: multiple processes can each allocate one frequency
% as long as there are frequencies available to allocate

%% flush the mailbox and print any messages to the consola
-spec clear() -> ok.
clear() ->
  receive
    Msg ->
      io:format("clear ~p~n", [Msg]),
      clear()
  after
    0 ->
      ok
  end.

%% These are the start functions used to create and
%% initialize the server.

-spec start() -> ok.
start() ->
  register(frequency, spawn(frequency, init, [])).

-spec init() -> ok.
init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
-spec get_frequencies() -> [integer()].
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

-spec loop({[integer()],[integer()]}) -> ok.
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

%% Functional interface

% Allocate a frequency
%
% First call clear/0 to ensure the mailbox is empty before sending allocate request message.
% Any unread messages will be printed to the console and discarded.
%
% Next send allocate request message allocate/2 and expect a reply.
%
% If a reply is received within 1000ms, return the reply, else return timeout to the client.
%
% A timeout may result in a late reply message left in the mailbox without the client's
% knowledge.
-spec allocate() -> ok.
allocate() ->
  clear(),
  frequency ! {request, self(), allocate},
  receive
    {reply, Reply} ->
      Reply
  after
    1000 ->
      timeout
  end.

% Deallocate a frequency
%
% Similar semantics as allocate/0 with possible left over messages in mailbox,
% possible timeout on reply, possible late reply left in mailbox.
-spec deallocate(integer()) -> ok.
deallocate(Freq) ->
  clear(),
  frequency ! {request, self(), {deallocate,Freq}},
  receive
    {reply, Reply} ->
      Reply
  after
    1000 ->
      timeout
  end.

% Stop the frequency server
%
% Send stop message to frequency server, cause loop/1 to exit.
%
% Similar semantics as allocate/0 with possible left over messages in mailbox,
% possible timeout on reply, possible late reply left in mailbox.
-spec stop() -> ok.
stop() ->
  clear(),
  frequency ! {request, self(), stop},
  receive
    {reply, Reply} ->
      Reply
  after
    1000 ->
      timeout
  end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

% Allocate a frequency
%
% Return {error, reason} if the caller already has an allocated
% frequency or if there is no available frequency to allocate.
%
% Otherwise, return {ok, Frequency}.
-spec allocate({[integer()],[integer()]}, pid()) -> ok.
allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  % simulate slow response by sleeping for 2000ms
  ?Overload,
  case lists:keymember(Pid,2,Allocated) of
    true ->
      {{[Freq|Free], Allocated}, {error, has_frequency}};
    false ->
      {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}
  end.

% Deallocate a frequency only if caller has the allocated frequency.
-spec deallocate({[integer()],[integer()]}, integer(), pid()) -> ok.
deallocate({Free, Allocated}, Freq, Pid) ->
  % simulate slow response by sleeping for 2000ms
  ?Overload,
  case lists:member({Freq,Pid},Allocated) of
    true ->
      NewAllocated=lists:keydelete(Freq, 1, Allocated),
      {{[Freq|Free],  NewAllocated}, ok};
    false ->
      {{Free, Allocated}, {error, not_allocated}}
  end.

-ifdef(Testing).
% test allocate/deallocate one frequency
-spec one_client_test() -> ok.
one_client_test() ->
  start(),
  io:format("~p allocating~n", [self()]),
  {ok, Freq}=allocate(),
  io:format("~p allocated ~p~n", [self(),Freq]),
  ?assertEqual(10, Freq),
  io:format("~p deallocating ~p~n", [self(),Freq]),
  ok=deallocate(Freq),
  io:format("~p deallocated~n", [self()]),
  stop(),
  ok.

% test allocate/deallocate multiple frequencies
% spawn multiple processes each of which will allocate and deallocate a frequency
-spec multiple_client_test() -> ok.
multiple_client_test() ->
  % start the frequency server
  start(),
  PID=self(),
  % Create a function to spawn a child that will
  % 1. allocate a freqency,
  % 2. send an allocate message to the parent,
  % 3. wait for an ack from the parent,
  % 4. deallocate the frequency,
  % 5. exit
  F=fun(Freq) ->
        spawn(fun() ->
                  io:format("~p spawned ~p~n", [PID,self()]),
                  % assert that we got the expected frequency
                  ?assertEqual({ok, Freq}, allocate()),
                  io:format("~p allocated ~p~n", [self(),Freq]),
                  % send allocated message to parent
                  PID ! {self(),allocated,Freq},
                  % wait to deallocate
                  receive _ -> ok end,
                  % assert that we successfully deallocated the frequency
                  ?assertEqual(ok, deallocate(Freq)),
                  io:format("~p deallocated ~p~n", [self(),Freq]),
                  exit(0)
              end)
    end,
  % frequencies will be allocated in order of [10,11,12,13...]
  % spawn children passing expected frequency that will be allocated
  PID1=F(10),
  PID2=F(11),
  PID3=F(12),
  PID4=F(13),
  % wait for all children to allocate frequencies
  receive _ -> ok end,
  receive _ -> ok end,
  receive _ -> ok end,
  receive _ -> ok end,
  % send message to children to continue with deallocation
  PID4 ! ok,
  PID3 ! ok,
  PID2 ! ok,
  PID1 ! ok,
  % ppuse to let the children exit
  timer:sleep(500),
  % shutdown
  stop(),
  ok.
-endif.
