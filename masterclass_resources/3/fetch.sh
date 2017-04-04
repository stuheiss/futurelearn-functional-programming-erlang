#Supporting resources:

#General: 
#calc_gen.erl 
#expr.erl
#client1.erl 
#server1.erl

#Applications: 
#calc.erl 
#calc_sup.erl
#calc_app.erl
#expr.erl

#Behaviours: 
#calc.erl 
#expr.erl

#Gen_servers: 
#calc.erl
#expr.erl

#Server: 
#calc.erl 
#server.erl
#expr.erl

#Supervisors: 
#calc.erl
#expr.erl
#calc_sup.erl

base='https://www.cs.kent.ac.uk/ErlangMasterClasses/Erlang%20Master%20Class%203%20resources'
for d in applications behaviours gen_servers server supervisors;do test -d $d || mkdir $d;done

for i in calc_gen expr client1 server1; do f="$i.erl"; test -f $f && continue; echo "$f"; wget -O $f "$base/$f"; done
for i in calc calc_sup calc_app expr; do f="applications/$i.erl"; test -f $f && continue; echo "$f"; wget -O $f "$base/$f"; done
for i in calc expr; do f="behaviours/$i.erl"; test -f $f && continue; echo "$f"; wget -O $f "$base/$f"; done
for i in calc expr; do f="gen_servers/$i.erl"; test -f $f && continue; echo "$f"; wget -O $f "$base/$f"; done
for i in calc server expr; do f="server/$i.erl"; test -f $f && continue; echo "$f"; wget -O $f "$base/$f"; done
for i in calc expr calc_sup; do f="supervisors/$i.erl"; test -f $f && continue; echo "$f"; wget -O $f "$base/$f"; done
