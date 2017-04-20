# see https://erlang.mk/guide/getting_started.html

# download
wget https://erlang.mk/erlang.mk

# init Makefile
echo "include erlang.mk" > Makefile

# list templates
make list-templates

# create a gen_server template for module my_server
make new t=gen_server n=my_server

