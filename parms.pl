/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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
	  [ serql_setting/3
	  ]).
:- use_module(library(settings)).
:- use_module(library(error)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_hook)).


		 /*******************************
		 *	    FILE PATHS		*
		 *******************************/

:- multifile
	user:file_search_path/2.

user:file_search_path(library,	     cliopatria(lib)).
user:file_search_path(ontology_root, cliopatria('ontologies')).
user:file_search_path(css,	     cliopatria('web/css')).
user:file_search_path(icons,	     cliopatria('web/icons')).
user:file_search_path(yui,	     cliopatria('web/yui/2.7.0')).
user:file_search_path(js,	     cliopatria('web/js')).


		 /*******************************
		 *	     HTTP PATHS		*
		 *******************************/

http:location(cliopatria, root(.),	  []).
http:location(sesame,	  root(servlets), []).
http:location(sparql,	  root(sparql),	  []).


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
		 *	      SETTINGS		*
		 *******************************/

%%	serql_setting(+Name, -Old, +New)
%
%	Change a setting.  Not  all   settings  are  changeable  without
%	restarting the server.
%
%	@tbd	Handle setting broadcasts to update server statistics

serql_setting(Name, Old, New) :-
	setting(Name, Old),
	set_setting(Name, New).

		 /*******************************
		 *	       HTTP		*
		 *******************************/

:- setting(http:port, nonneg, env('PORT', 3020),
	   'Port the http server listens to').
:- setting(http:workers, between(1, 20), env('SERQL_WORKERS', 5),
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
		 *	   OTHER SETTINGS	*
		 *******************************/

:- setting(cliopatria:user_data, atom, 'users.db',
	   'File holding account information').
:- setting(cliopatria:default_entailment, atom, rdfs,
	   'Default entailment rules applied').
:- setting(cliopatria:optimise_query, boolean, true,
	   'Optimise queries before execution').
:- setting(cliopatria:rdf_db_namespaces, boolean, true,
	   'Allow registered namespaces in queries').
:- setting(cliopatria:title, atom, 'ClioPatria Semantic Web Server',
	   'Title of the web-page').
:- setting(cliopatria:persistent_store, atom, 'RDF-store',
	   'Directory for persistent copy of in-memory RDF').
:- setting(cliopatria:base_ontologies, list(any), [serql(rdfs)],
	   'Load these files into a virgin database').
