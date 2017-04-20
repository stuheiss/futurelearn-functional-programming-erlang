-module(counter_client).
-export([handle/2,start/1,tick/2,read/1,zero/1,stop/1]).
-import(counter_server, [start/3, rpc/2]).

%% specific client
start(Name) -> start(Name, ?MODULE, 0).

tick(Name,N) -> rpc(Name, {tick, N}).

read(Name) -> rpc(Name, read).

zero(Name) -> rpc(Name, zero).

stop(Name) -> exit(whereis(Name), ok).

%% callbacks
handle({tick,N}, State) -> {ack, State+N};
handle(read, State) -> {State, State};
handle(zero, _State) -> {ack, 0}.
