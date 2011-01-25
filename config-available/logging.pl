:- module(conf_logging, []).
:- use_module(library(settings)).

/** <module> Configure logging of HTTP traffic

Configure logging of HTTP traffic.

@tbd	Include programs to convert and process log-files.
*/

		 /*******************************
		 *	      LOGGING		*
		 *******************************/

% Log  HTTP  traffic  to  httpd.log.    Logging   may  seriously  affect
% performance on servers that handle many tiny requests.

:- use_module(library(http/http_log)).

% Set the default name of the logfile. Setting it to '' disables logging

% :- set_setting_default(http:logfile, 'cliopatria.log').
