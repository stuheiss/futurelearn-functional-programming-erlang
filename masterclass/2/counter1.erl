% refactor concurrent counter - non-concurrent part
-module(counter1).
-import(gen_server_lite,[start/2,rpc/2]).
-export([start/0,tick/1,read/0,stop/0,handle/2]).

start() -> start(counter1,0).

tick(N) -> rpc(counter1, {tick,N}).
read() -> rpc(counter1, read).
stop() -> rpc(counter1, stop).

handle({tick,N},State) -> {ack,State+N};
handle(read,State) -> {State,State}.
