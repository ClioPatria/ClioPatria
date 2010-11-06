:- module(conf_https, []).
:- use_module(library(settings)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

/** <module> Provide HTTPS

This plugin module makes the server available under https (aka http over
SSL).  The default port for HTTPS usage is 443.

@see etc/README.txt for creating SSL certificates
*/

:- setting(https:port, integer, 1443,
	   'Port to use for https connections').

:- set_setting_default(http:public_host,   localhost).
:- set_setting_default(http:public_port,   setting(https:port)).
:- set_setting_default(http:public_scheme, https).

start_https :-
	setting(https:port, Port),
	http_server(http_dispatch,
		    [ port(Port),
		      ssl([ host('localhost'),
                            cacert_file('etc/demoCA/cacert.pem'),
			    certificate_file('etc/server/server-cert.pem'),
			    key_file('etc/server/server-key.pem'),
			    password('apenoot1')
                          ])
                    ]).

:- initialization
	start_https.


