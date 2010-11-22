:- module(conf_network, []).
:- use_module(library(settings)).

/** <module> Configure the HTTP port

Change  the  default  port  on  which    the  HTTP  server  listens.  If
host-detection does not work or this server   is behind a proxy, you may
also need the public_host/public_port settings.

@see localhost.pl
*/

% :- set_setting_default(http:port, 3020).
% :- set_setting_default(http:public_host, 'www.example.org').
% :- set_setting_default(http:public_port, 80).

