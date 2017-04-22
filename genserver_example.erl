% genserver example
-module(genserver_example).
-behaviour(gen_server).
-export([start_link/0,funsync/1,funasync/1,stop/0]).
-export([init/1,handle_call/3,handle_cast/2]).
% add exports if you override defaults
-export([handle_info/2, terminate/2, code_change/3]).

% gen_server vsn
%% API
% use start_link() if no supervisor
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

funsync(Arg) ->
  gen_server:call(?MODULE, {atomsync, Arg}).

funasync(Arg) ->
  gen_server:cast(?MODULE, {atomasync, Arg}).

% use stop() if no supervisor
% can use gen_server:stop/1 or gen_server:cast/2
stop() ->
  %gen_server:cast(?MODULE, stop).
  gen_server:stop(?MODULE).

%% callbacks
init([]) ->
  State = 0,
  {ok, State}.

handle_call(Msg, _From, State) ->
  {NewState, {ok, Reply}} = dosomething(Msg, State),
  {reply, Reply, NewState}.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(Msg, State) ->
  {NewState, {ok, _Reply}} = dosomething(Msg, State),
  {noreply, NewState}.

dosomething(Arg, State) ->
  Reply = Arg,
  NewState = State,
  {NewState, {ok, Reply}}.

% default callback implementations
% can override with specific implementations

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
