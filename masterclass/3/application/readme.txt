An application has a name and vsn and directory structure:
  name=calc
  vsn=1.0
  dirs: calc-1.0/{ebin,src,priv,include}
hrl files go in include
erl files go in src
calc.app and beam files go in ebin
assets go in priv

An application has an app file ebin/<name>.app:
{application, <name>,
 [{descripton, "<desc>"},
  {vsn, "<vsn>"},
  {modules, [<name>, <name>_sup, <name>_app, <othername>]},
  {registered, [<name>_sup, <name>]},
  {applications, [kernel, stdlib]},
  {env, [{env, [{a, <v1>}, {b, <v2>}]}]},
  {mod, {<name>_app, []}}]}.

Usage:
application:start(<name>).
application:stop(<name>).
<name>:<function>(<args>).
application:get_all_env(<name>).
application:which_applications().
whereis(<name>).
<name>:stop().

An application can be bundled into a release (<name>.rel):
{release
  {"OTP APN 181 01","R14B02"},
  {erts, "5.8.3"},
  [{kernel, "2.14.3"},
   {stdlib, "1.17.3"},
   {calc, "1.0"}]}.

