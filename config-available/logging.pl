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
% Creating the logfile in a directory is preferred if the service is not
% running under the same user as the owner of the sources.

:- set_setting_default(http:logfile, 'log/httpd.log').
