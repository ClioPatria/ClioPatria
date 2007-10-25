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
:- use_module(library('http/http_parameters')).
:- use_module(library('http/http_dispatch')).
:- use_module(sparql).
:- use_module(sparql_xml_result).

:- http_handler('/sparql/', sparql_reply, []).

%%	sparql_reply(+Request)
%
%	HTTP handler for SPARQL requests.  Typically mounted on
%	=|/sparql/|=

sparql_reply(Request) :-
	http_parameters(Request,
			[ query(Query),
			  'default-graph-uri'(DefaultGraphs),
			  'named-graph-uri'(NamedGraphs)
			],
			[ attribute_declarations(sparql_decl)
			]),
	append(DefaultGraphs, NamedGraphs, AllGraphs),
	authorized(read(AllGraphs, query)),
	statistics(cputime, CPU0),
	sparql_compile(Query, Compiled,
		       [ type(Type),
			 ordered(Ordered),
			 distinct(Distinct)
		       ]),
	findall(R, sparql_run(Compiled, R), Rows),
	statistics(cputime, CPU1),
	CPU is CPU1 - CPU0,
	write_result(Type, Rows,
		     [ cputime(CPU),
		       ordered(Ordered),
		       distinct(Distinct)
		     ]).
			
write_result(ask, [True], Options) :- !,
	format('Content-type: application/sparql-result+xml~n~n'),
	sparql_write_xml_result(current_output, ask(True), Options).
write_result(select(VarNames), Rows, Options) :- !,
	format('Content-type: application/sparql-result+xml~n~n'),
	sparql_write_xml_result(current_output, select(VarNames, Rows), Options).
write_result(_, RDF, _Options) :-
	format('Content-type: application/rdf+xml; charset=UTF-8~n~n'),
	rdf_write_xml(current_output, RDF).


%%	sparql_decl(+OptionName, -Options)
%	
%	Default   options   for   specified     attribute   names.   See
%	http_parameters/3.

sparql_decl(query,
	    []).
sparql_decl('default-graph-uri',
 	    [ zero_or_more
	    ]).
sparql_decl('named-graph-uri',
 	    [ zero_or_more
	    ]).
