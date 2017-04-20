% genserver example
-module(genserver_example).
-behaviour(gen_server).
-export([start_link/0,funsync/1,funasync/1,stop/0]).
-export([init/1,handle_call/3,handle_cast/2]).

% gen_server vsn
%% API
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

funsync(Arg) ->
  gen_server:call(?MODULE, {atomsync, Arg}).

funasync(Arg) ->
  gen_server:cast(?MODULE, {atomasync, Arg}).

stop() ->
  gen_server:stop(?MODULE).

%% callbacks
init([]) ->
  State = 0,
  {ok, State}.

handle_call(Msg, _From, State) ->
  {Reply, NewState} = dosomething(Msg, State),
  {reply, Reply, NewState}.

handle_cast(Msg, State) ->
  {_Reply, NewState} = dosomething(Msg, State),
  {noreply, NewState}.

dosomething(Arg, State) ->
  Reply = Arg,
  NewState = State,
  {Reply, NewState}.

