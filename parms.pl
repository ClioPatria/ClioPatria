/*  Part of ClioPatria semantic web server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008-2010, University of Amsterdam
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(cp_parms,
	  [
	  ]).
:- use_module(library(settings)).
:- use_module(library(error)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_hook)).

/** <module> ClioPatria parameters

This file contains the locations of file-directories and web-directories
in addition to settings that can be   managed by the end-user. It should
not be necessary to modify this file:

    * Web-locations can be modified externally using http:location/3
    with an option priority(N), where N > 0. See http_absolute_path/3.

    * Settings can be changed using set_setting_default/2.

@see run.pl[.in] contains an example startup script.
*/

		 /*******************************
		 *	    FILE PATHS		*
		 *******************************/

:- multifile
	user:file_search_path/2.

% ClioPatria specific ones
user:file_search_path(rdfql,	        cliopatria(rdfql)).
user:file_search_path(cpack,	        cliopatria(cpack)).

% Package merge
user:file_search_path(cpacks,	        cliopatria('.')).

user:file_search_path(library,		cpacks(lib)).
user:file_search_path(rdf,		cpacks(rdf)).
user:file_search_path(entailment,	cpacks(entailment)).
user:file_search_path(components,	cpacks(components)).
user:file_search_path(applications,	cpacks(applications)).
user:file_search_path(api,		cpacks(api)).
user:file_search_path(user,		cpacks(user)).
user:file_search_path(config_available,	cpacks('config-available')).
user:file_search_path(skin,		cpacks(skin)).
user:file_search_path(web,		cpacks(web)).
user:file_search_path(css,		web(css)).
user:file_search_path(icons,		web(icons)).
user:file_search_path(yui,		web('yui/2.7.0')).
user:file_search_path(js,		web(js)).
user:file_search_path(html,		web(html)).
user:file_search_path(help,		web(help)).
user:file_search_path(tutorial,		web(tutorial)).


		 /*******************************
		 *	     HTTP PATHS		*
		 *******************************/

http:location(cliopatria,  root(.),	       []).
http:location(sesame,	   root(servlets),     []).
http:location(sparql,	   root(sparql),       []).
http:location(rdf_browser, cliopatria(browse), []).
http:location(api,	   cliopatria(api),    []).
http:location(json,	   api(json),	       []).


		 /*******************************
		 *	       TYPES		*
		 *******************************/

%	make the type 'uri' work such  that   we  can  write NS:Local in
%	settings defaults.

:- multifile
	error:has_type/2,
	settings:eval_default/3,
	settings:convert_text/3,
	http_settings:input_item/5.

error:has_type(uri, X) :-		% URI is an atom.  We do not use atom
	atom(X).			% to allow for conversion

					% Convert NS:Local
settings:eval_default(URI, uri, URI) :-
	atom(URI), !.
settings:eval_default(NS:Local, uri, URI) :-
	rdf_global_id(NS:Local, URI).

settings:convert_text(uri, Text, URI) :- !,
	(   sub_atom(Text, B, _, A, :),
	    sub_atom(Text, 0, B, _, NS),
	    sub_atom(Text, _, A, 0, Local),
	    identifier(NS),
	    identifier(Local)
	->  rdf_global_id(NS:Local, URI)
	;   URI = Text
	).

%%	identifier(+Atom) is semidet.
%
%	True if Atom contains only alphanumerical characters or
%	undercores.
%
%	@tbd	Must we support unicode here?  Check with Turtle.

identifier(Text) :-
	atom_codes(Text, Codes),
	identifier_codes(Codes).

identifier_codes([]).
identifier_codes([H|T]) :-
	identifier_code(H),
	identifier_codes(T).

identifier_code(C) :-
	code_type(C, csym).

					% Render as plain atom
http_settings:input_item(uri, Value, Name) -->
	html(input([name(Name), size(40), value(Value)])).


		 /*******************************
		 *	       HTTP		*
		 *******************************/

:- setting(http:port, nonneg, env('PORT', 3020),
	   'Port the http server listens to').
:- setting(http:workers, between(1, 20), env('PROLOG_HTTP_WORKERS', 5),
	   'Number of server threads').
:- setting(http:worker_options, list(any), [],
	   'Additional options to pass to the HTTP server').
:- setting(http:max_idle_time, nonneg, 3600,
	   'Session timeout.  If 0, session never times out').
:- setting(http:server_url, atom, 'http://localhost:'+setting(http:port),
	   'Url of the server itself').
:- setting(http:prefix, atom, '',
	   'Prefix to rebase the server').


		 /*******************************
		 *      CLIOPATRIA SETTINGS	*
		 *******************************/

:- setting(cliopatria:user_data, atom, 'users.db',
	   'File holding account information').
:- setting(cliopatria:default_entailment, atom, rdfs,
	   'Default entailment rules applied').
:- setting(cliopatria:optimise_query, boolean, true,
	   'Optimise queries before execution').
:- setting(cliopatria:rdf_db_namespaces, boolean, true,
	   'Allow registered namespaces in queries').
:- setting(cliopatria:persistent_store, atom, '',
	   'Directory for persistent copy of in-memory RDF').
:- setting(cliopatria:pre_index_tokens, boolean, false,
	   'Build the fulltext token index while loading').
:- setting(cliopatria:pre_index_stems, boolean, false,
	   'Build the fulltext stem index while loading').


		 /*******************************
		 *	  QUERY-SETTINGS	*
		 *******************************/

:- setting(sparql:max_clients, nonneg, 100,
	   'Maximum number of concurrent requests').
:- setting(sparql:stack_size, nonneg, 1000,
	   'Size of the global stack in mega-bytes').
