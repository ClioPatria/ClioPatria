:- module(conf_network, []).
:- use_module(library(settings)).

/** <module> Configure the HTTP port

Change the default port on which the HTTP server listens.

@see localhost.pl
*/

% :- set_setting_default(http:port, 3020).

