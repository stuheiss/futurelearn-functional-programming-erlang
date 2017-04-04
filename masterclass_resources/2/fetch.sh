base='https://www.cs.kent.ac.uk/ErlangMasterClasses/Erlang%20Master%20Class%202%20resources'
for i in calc gen_server_lite client1 lecture2 counter0 par_server counter1 server1 demo1 triv_tcp_fac_client demo1_1 triv_tcp_fac_server demo2 triv_tcp_name_server demo2_1 triv_tcp_resolver elib
do
  f="$i.erl"; test -f $f && continue; wget -O $f "$base/$f"
done
