:- module(conf_debug, [ tmon/0 ]).
:- use_module(library(debug)).

/** <module> Set options for development

@see	http://www.swi-prolog.org/howto/http/Developing.html
*/

% Turn uncaught exceptions  into  a  500   error-page  that  includes  a
% stack-dump. Handy for debugging, but also  handy for someone trying to
% hack your server :-(

:- use_module(library(http/http_error)).

% Make the toplevel and debugger print RDF resources as prefix:local.

:- use_module(library(semweb/rdf_portray)).

% Print HTTP requests to the console (handy for debugging. Typically you
% want to disable this on an operational server.

:- debug(http(request)).

%%	tmon
%
%	Show the graphical thread-monitor. Can be  useful to examine and
%	debug active goals.

tmon :-
	call(prolog_ide(thread_monitor)).
