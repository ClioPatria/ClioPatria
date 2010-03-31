/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2006, University of Amsterdam

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

:- module(http_sparql,
	  [ sparql_reply/1
	  ]).
:- use_module(user_db).
:- use_module(library(lists)).
:- use_module(library(rdf_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_request_value)).
:- use_module(sparql).
:- use_module(sparql_xml_result).
:- use_module(sparql_json_result).

:- http_handler(sparql(.), sparql_reply, [spawn(sparql_query)]).

%%	sparql_reply(+Request)
%
%	HTTP handler for SPARQL requests.  Typically mounted on
%	=|/sparql/|=

sparql_reply(Request) :-
	http_parameters(Request,
			[ query(Query),
			  'default-graph-uri'(DefaultGraphs),
			  'named-graph-uri'(NamedGraphs),
			  format(ReqFormat),
			  entailment(Entailment)
			],
			[ attribute_declarations(sparql_decl)
			]),
	append(DefaultGraphs, NamedGraphs, AllGraphs),
	authorized(read(AllGraphs, query)),
	statistics(cputime, CPU0),
	sparql_compile(Query, Compiled,
		       [ type(Type),
			 ordered(Ordered),
			 distinct(Distinct),
			 entailment(Entailment)
		       ]),
	findall(R, sparql_run(Compiled, R), Rows),
	statistics(cputime, CPU1),
	CPU is CPU1 - CPU0,
	output_format(ReqFormat, Request, Format),
	write_result(Format, Type, Rows,
		     [ cputime(CPU),
		       ordered(Ordered),
		       distinct(Distinct)
		     ]).

output_format(ReqFormat, Request, Format) :-
	var(ReqFormat), !,
	accept_output_format(Request, Format).
output_format('rdf+xml', _, xml).
output_format(json, _, json).

accept_output_format(Request, Format) :-
	memberchk(accept(Accept), Request),
	http_parse_header_value(accept, Accept, Media),
	find_media(Media, Format), !.
accept_output_format(_, xml).

find_media([media(Type, _, _, _)|T], Format) :-
	(   sparql_media(Type, Format)
	->  true
	;   find_media(T, Format)
	).

sparql_media(application/'sparql-result+xml',   xml).
sparql_media(application/'sparql-results+json', json).

write_result(xml, Type, Rows, Options) :-
	write_xml_result(Type, Rows, Options).
write_result(json, Type, Rows, Options) :-
	write_json_result(Type, Rows, Options).

write_xml_result(ask, [True], Options) :- !,
	format('Content-type: application/sparql-result+xml; charset=UTF-8~n~n'),
	sparql_write_xml_result(current_output, ask(True), Options).
write_xml_result(select(VarNames), Rows, Options) :- !,
	format('Transfer-encoding: chunked~n'),
	format('Content-type: application/sparql-result+xml; charset=UTF-8~n~n'),
	sparql_write_xml_result(current_output, select(VarNames, Rows), Options).
write_xml_result(_, RDF, _Options) :-
	format('Content-type: application/rdf+xml; charset=UTF-8~n~n'),
	rdf_write_xml(current_output, RDF).

write_json_result(ask, [True], Options) :- !,
	sparql_write_json_result(current_output, ask(True), Options).
write_json_result(select(VarNames), Rows, Options) :- !,
	format('Transfer-encoding: chunked~n'),
	sparql_write_json_result(current_output, select(VarNames, Rows), Options).
write_json_result(_, _RDF, _Options) :-
	throw(http_reply(bad_request(format('JSON output is only supported for \
					     ASK and SELECT queries', [])))).


%%	sparql_decl(+OptionName, -Options)
%
%	Default   options   for   specified     attribute   names.   See
%	http_parameters/3.

sparql_decl(query,
	    [ description('The SPARQL query to execute')
	    ]).
sparql_decl('default-graph-uri',
 	    [ list(atom),
	      description('The default graph(s) to query (not supported)')
	    ]).
sparql_decl('named-graph-uri',
 	    [ list(atom),
	      description('Additional named graph(s) to query (not supported)')
	    ]).
sparql_decl(format,
 	    [ optional(true),
	      oneof(['rdf+xml', json]),
	      description('Result format.  If not specified, the \
	      		  HTTP Accept header is used')
	    ]).
sparql_decl(entailment,
 	    [ optional(true),
	      default(rdf),
	      oneof(Es),
	      description('Entailment used')
	    ]) :-
	findall(E, serql:entailment(E, _), Es).

