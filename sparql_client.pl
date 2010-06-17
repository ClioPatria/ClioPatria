/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2010, University of Amsterdam
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(sparql_client,
	  [ sparql_query/3,		% +Query, -Row, +Options
	    sparql_set_server/1		% +Options
	  ]).
:- use_module(library(http/http_open)).
:- use_module(sparql_xml_result).
:- use_module(library(lists)).
:- use_module(library(rdf)).
:- use_module(library(option)).

/** <module> SPARQL client library

This module provides a SPARQL client.  For example:

    ==
    ?- sparql_query('select * where { ?x rdfs:label "Amsterdam" }', Row,
		    [ host('dbpedia.org'), path('/sparql')]).

    Row = row('http://www.ontologyportal.org/WordNet#WN30-108949737') ;
    false.
    ==

Or, querying a local server using an =ASK= query:

    ==
    ?- sparql_query('ask { owl:Class rdfs:label "Class" }', Row,
		    [ host('localhost'), port(3020), path('/sparql/')]).
    true.
    ==
*/


%%	sparql_query(+Query, -Result, +Options) is nondet.
%
%	Execute a SPARQL query on an HTTP   SPARQL endpoint. Query is an
%	atom that denotes  the  query.  Result   is  unified  to  a term
%	rdf(S,P,O) for =construct= queries  and   row(...)  for =select=
%	queries.  Options are
%
%	    * host(+Host)
%	    * port(+Port)
%	    * path(+Path)
%	    * search(+ListOfParams)
%
%	Remaining options are passed to   http_open/3.  The defaults for
%	Host, Port and Path can be   set  using sparql_set_server/1. The
%	initial default for port is 80 and path is =|/sparql/|=.

sparql_query(Query, Row, Options) :-
	sparql_param(host(Host), Options,  Options1),
	sparql_param(port(Port), Options1, Options2),
	sparql_param(path(Path), Options2, Options3),
	select_option(search(Extra), Options3, Options4, []),
	http_open([ protocol(http),
		    host(Host),
		    port(Port),
		    path(Path),
		    search([ query = Query
			   | Extra
			   ])
		  | Options4
		  ], In,
		  [ header(content_type, ContentType)
		  ]),
	plain_content_type(ContentType, CleanType),
	read_reply(CleanType, In, Row).

read_reply('application/rdf+xml', In, Row) :- !,
	call_cleanup(load_rdf(stream(In), RDF), close(In)),
	member(Row, RDF).
read_reply(MIME, In, Row) :-
	sparql_result_mime(MIME), !,
	call_cleanup(sparql_read_xml_result(stream(In), Result),
		     close(In)),
	xml_result(Result, Row).
read_reply(Type, In, _) :-
	read_stream_to_codes(In, Codes),
	string_to_list(Reply, Codes),
	close(In),
	throw(error(domain_error(sparql_result_document, Type),
		    context(_, Reply))).

sparql_result_mime('application/sparql-results+xml').
sparql_result_mime('application/sparql-result+xml').


plain_content_type(Type, Plain) :-
	sub_atom(Type, B, _, _, (;)), !,
	sub_string(Type, 0, B, _, Main),
	normalize_space(atom(Plain), Main).
plain_content_type(Type, Type).

xml_result(ask(Bool), Result) :- !,
	Result = Bool.
xml_result(select(_VarNames, Rows), Result) :-
	member(Result, Rows).





		 /*******************************
		 *	      SETTINGS		*
		 *******************************/

:- dynamic
	sparql_setting/1.

sparql_setting(port(80)).
sparql_setting(path('/sparql/')).

sparql_param(Param, Options0, Options) :-
	select_option(Param, Options0, Options), !.
sparql_param(Param, Options, Options) :-
	sparql_setting(Param), !.
sparql_param(Param, Options, Options) :-
	functor(Param, Name, _),
	throw(error(existence_error(option, Name), _)).

%%	sparql_set_server(+OptionOrList)
%
%	Set sparql server default options.  Provided defaults are:
%	host, port and repository.  For example:
%
%	    ==
%		set_sparql_default([ host(localhost),
%				     port(8080)
%				     repository(world)
%				   ])
%	    ==

sparql_set_server([]) :- !.
sparql_set_server([H|T]) :- !,
	sparql_set_server(H),
	sparql_set_server(T).
sparql_set_server(Term) :-
	functor(Term, Name, Arity),
	functor(Unbound, Name, Arity),
	retractall(sparql_setting(Unbound)),
	assert(sparql_setting(Term)).
