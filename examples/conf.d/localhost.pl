:- module(conf_localhost, []).
:- use_module(library(settings)).

/** <module> Configure HTTP to stay on localhost

If you login, the system will redirect  you to its public address. I.e.,
if you connected to  http://localhost:3020/  it   will  redirect  you to
http://my.domain.org:3020/. This can be undesirable  on e.g., a notebook
that is not always connected to the   internet and/or may change address
and/or may be behind a firewall. You   can disable redirection using the
settings below. These settings may also be   necessary  if the server is
behind a proxy.

@see network.pl for changing the port
*/

:- set_setting_default(http:public_host, localhost).
:- set_setting_default(http:public_port, setting(http:port)).

