/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2010, University of Amsterdam,
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

:- module(api_sparql,
	  [
	  ]).
:- use_module(user(user_db)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(uri)).
:- use_module(library(rdf_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_request_value)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/html_write)).
:- use_module(rdfql(sparql)).
:- use_module(rdfql(sparql_xml_result)).
:- use_module(rdfql(sparql_json_result)).
:- use_module(rdfql(sparql_csv_result)).
:- use_module(library(settings)).
:- if(exists_source(applications(yasgui))).
:- use_module(applications(yasgui)).
:- endif.

:- http_handler(sparql(.),      sparql_query,  [spawn(sparql_query), id(sparql_query)]).
:- http_handler(sparql(update), sparql_update, [spawn(sparql_query), id(sparql_update)]).

%%	sparql_query(+Request)
%
%	HTTP  handler  for  SPARQL  requests.    Mounted  the  http-path
%	sparql(.)       (by       default        =|/sparql/|=,       see
%	library(http/http_path)).

sparql_query(Request) :-
	empty_get_request(Request), !,
	redirect_human_form(Request).
sparql_query(Request) :-
	http_parameters(Request,
			[ query(Query),
			  'default-graph-uri'(DefaultGraphs),
			  'named-graph-uri'(NamedGraphs),
			  format(ReqFormat),
			  entailment(Entailment)
			],
			[ attribute_declarations(sparql_decl)
			]),
	append(DefaultGraphs, NamedGraphs, Graphs),
	authorized(read(Graphs, sparql)),
	sparql_reply(Request, Query, Graphs, ReqFormat, Entailment).

%%	empty_get_request(+Request) is semidet.
%
%	True if Request is an HTTP GET request without any parameters.

empty_get_request(Request) :-
	option(request_uri(URI), Request),
	uri_components(URI, Components),
	uri_data(search, Components, Search),
	var(Search),
	option(method(get), Request).

:- if(current_predicate(has_yasgui/0)).
human_form_location(HREF) :-
	has_yasgui, !,
	http_link_to_id(yasgui, [], HREF).
:- endif.
human_form_location(HREF) :-
	http_link_to_id(sparql_query_form, [], HREF).

redirect_human_form(Request) :-
	human_form_location(HREF),
	reply_html_page(cliopatria(default),
			[ title('Redirect to SPARQL editor'),
			  meta([ 'http-equiv'(refresh),
				 content('5; url='+HREF)
			       ])
			], \sparql_redirect_explanation(Request, HREF)).

sparql_redirect_explanation(Request, EditorHREF) -->
	{ option(request_uri(URI), Request) },
	html({|html(URI, EditorHREF)||
<h4>Redirecting to SPARQL editor ...</h4>

<div class="warning" style="width:80%;margin:auto;border:1px solid #888;padding: 10px 5px">
You have landed in the SPARQL access location <a href=URI>URI</a> of this server.
<b>This URI is intended for machines</b>.  Because your request contains no parameters,
you will be redirected to the SPARQL editor at <a href=EditorHREF>EditorHREF</a>
in 5 seconds.
</div>
	     |}).



%%	sparql_update(+Request)
%
%	HTTP handler for SPARQL update  requests.   This  is the same as
%	query requests, but the takes the   query  in the =update= field
%	rather than in the =query= field.

% Browser pointed here
sparql_update(Request) :-
	empty_get_request(Request), !,
	redirect_human_form(Request).
% Perform a SPARQL update via POST directly.
% @compat SPARQL 1.1 Protocol recommendation, section 2.2.2.
sparql_update(Request) :-
	memberchk(content_type(ContentType), Request),
	sub_atom(ContentType, 0, _, _, 'application/sparql-update'), !,
	http_parameters(Request,
			[ 'using-graph-uri'(DefaultGraphs),
			  'using-named-graph-uri'(NamedGraphs),
			  format(ReqFormat),
			  entailment(Entailment)
			],
			[attribute_declarations(sparql_decl)
			]),
	append(DefaultGraphs, NamedGraphs, Graphs),
	http_read_data(Request, Query, []),
	authorized(write(Graphs, sparql)),
	sparql_reply(Request, Query, Graphs, ReqFormat, Entailment).
% Perform a SPARQL update via POST with URL-encoded parameters.
% @compat SPARQL 1.1 Protocol recommendation, section 2.2.1.
sparql_update(Request) :-
	http_parameters(Request,
			[ update(Query),
			  'using-graph-uri'(DefaultGraphs),
			  'using-named-graph-uri'(NamedGraphs),
			  format(ReqFormat),
			  entailment(Entailment)
			],
			[ attribute_declarations(sparql_decl)
			]),
	append(DefaultGraphs, NamedGraphs, Graphs),
	authorized(write(Graphs, sparql)),
	sparql_reply(Request, Query, Graphs, ReqFormat, Entailment).


%%	sparql_reply(+Request, +Query, +_Graphs, +ReqFormat, +Entailment)
%
%	HTTP  handler  for  SPARQL  requests.    Mounted  the  http-path
%	sparql(.)       (by       default        =|/sparql/|=,       see
%	library(http/http_path)).

sparql_reply(Request, Query, Graphs, ReqFormat, Entailment) :-
	statistics(cputime, CPU0),
	sparql_compile(Query, Compiled,
		       [ type(Type),
			 ordered(Ordered),
			 distinct(Distinct),
			 entailment(Entailment)
		       ]),
	(   Compiled = sparql_query(update(_), _, _)
	->  authorized(write(Graphs, sparql))
	;   true
	),
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
output_format('rdf+xml', _, xml) :- !.
output_format(json, _, json) :- !.
output_format(csv, _, csv) :- !.
output_format(Mime, _, Format) :-
	atomic_list_concat([Major,Minor], /, Mime),
	sparql_media(Major/Minor, Format), !.


accept_output_format(Request, Format) :-
	memberchk(accept(Accept), Request),
	(   atom(Accept)
	->  http_parse_header_value(accept, Accept, Media)
	;   Media = Accept
	),
	find_media(Media, Format), !.
accept_output_format(_, xml).

find_media([media(Type, _, _, _)|T], Format) :-
	(   sparql_media(Type, Format)
	->  true
	;   find_media(T, Format)
	).

sparql_media(application/'sparql-results+xml',   xml).
sparql_media(application/'sparql-results+json', json).
sparql_media(text/'tab-separated-values',	 csv).

write_result(xml, Type, Rows, Options) :-
	cors_enable,
	write_xml_result(Type, Rows, Options).
write_result(json, Type, Rows, Options) :-
	cors_enable,
	write_json_result(Type, Rows, Options).
write_result(csv, Type, Rows, Options) :-
	cors_enable,
	write_csv_result(Type, Rows, Options).

write_xml_result(ask, [True], Options) :- !,
	format('Content-type: application/sparql-results+xml; charset=UTF-8~n~n'),
	sparql_write_xml_result(current_output, ask(True), Options).
write_xml_result(update, [True], Options) :- !,
	format('Content-type: application/sparql-results+xml; charset=UTF-8~n~n'),
	sparql_write_xml_result(current_output, update(True), Options).
write_xml_result(select(VarNames), Rows, Options) :- !,
	format('Transfer-encoding: chunked~n'),
	format('Content-type: application/sparql-results+xml; charset=UTF-8~n~n'),
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
	throw(http_reply(bad_request(format('JSON output is only supported for \c
					     ASK and SELECT queries', [])))).

write_csv_result(select(VarNames), Rows, Options) :- !,
	format('Transfer-encoding: chunked~n'),
	sparql_write_csv_result(current_output, select(VarNames, Rows), Options).
write_csv_result(_, _RDF, _Options) :-
	throw(http_reply(bad_request(format('CSV output is only supported for \c
					     SELECT queries', [])))).


%%	sparql_decl(+OptionName, -Options)
%
%	Default   options   for   specified     attribute   names.   See
%	http_parameters/3.

sparql_decl(query,
	    [ description('The SPARQL query to execute')
	    ]).
sparql_decl(update,
	    [ description('The SPARQL update query to execute')
	    ]).
sparql_decl('default-graph-uri',
	    [ list(atom),
	      description('The default graph(s) to query (not supported)')
	    ]).
sparql_decl('named-graph-uri',
	    [ list(atom),
	      description('Additional named graph(s) to query (not supported)')
	    ]).
sparql_decl('using-graph-uri',
	    [ list(atom),
	      description('The default graph(s) to update (not supported)')
	    ]).
sparql_decl('using-named-graph-uri',
	    [ list(atom),
	      description('Additional named graph(s) to update (not supported)')
	    ]).
sparql_decl(format,
	    [ optional(true),
	      oneof([ 'rdf+xml',
		      json,
		      csv,
		      'application/sparql-results+xml',
		      'application/sparql-results+json'
		    ]),
	      description('Result format.  If not specified, the \c
			  HTTP Accept header is used')
	    ]).
sparql_decl(entailment,
	    [ optional(true),
	      default(Default),
	      oneof(Es),
	      description('Entailment used')
	    ]) :-
	setting(sparql:entailment, Default),
	findall(E, cliopatria:entailment(E, _), Es).

