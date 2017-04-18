% start erlang with a shortname and shared cookie to create a cluster
% erl -sname <node> -setcokie <cookie>
erl -sname ant1 -setcokie bee
erl -sname ant2 -setcokie bee
erl -sname ant3 -setcokie bee
% nodes by default will be connected to all other nodes
% it is possible to manually create nodes that are only connected to some nodes
% shortname nodes communicate on local network
% longname nodes communicate globally and can use dns (-sname or -name?)
% you cannot mix short and long in the same cluster
erl -sname ant1@1.2.3.4 -setcokie bee
erl -sname ant1@foo.com -setcokie bee
% a node runs on a host and is referred to by <name>@<host>
% 'erl -sname ant -setcokie bee' running on host baz is ant@baz (-sname or -name?)

% messages
% send message to PID as usual
% Pid ! Message
% send message to named process on node
% {'elf', 'ant@baz'} ! Message

% spawn a process on another node
% the code for the module must be available and compiled on the other node
spawn('ant@baz', M, F, A)
% to name a process on another node, you must register it on the other node

% runtime/misc
% start/stop local runtime systems with net_kernel utilities
% change cookie dynamically with erlang:setcookie
% get node info with node/0 and nodes/0
