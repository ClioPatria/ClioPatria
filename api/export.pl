/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, VU University Amsterdam

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

:- module(api_export, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_schema)).
:- use_module(rdfql(rdf_io)).
:- use_module(rdfql(rdf_turtle_io)).
:- use_module(user(user_db)).

:- http_handler(api(export_graph),         export_graph,  []).
:- http_handler(api(export_graph_schema),  export_graph_schema,  []).

/** <module> Export data from the server

*/

%%	export_graph(+Request)
%
%	Export a named graph in a   given  serialization. Whether or not
%	exporting of a named graph is  defined by authorized/1 using the
%	term:
%
%		* read(default, download(Graph))

export_graph(Request) :-
	http_parameters(Request,
			[ graph(Graph),
			  format(Format),
			  mimetype(Mime)
			],
			[ attribute_declarations(http_param)
			]
		       ),
	authorized(read(default, download(Graph))),
	send_graph(Graph, Format, Mime).

send_graph(Graph, Format, default) :- !,
	default_mime_type(Format, MimeType),
	send_graph(Graph, Format, MimeType).
send_graph(Graph, Format, MimeType) :- !,
	format('Transfer-Encoding: chunked~n'),
	format('Content-type: ~w; charset=UTF8~n~n', [MimeType]),
	send_graph(Graph, Format).

send_graph(Graph, turtle) :- !,
	rdf_save_turtle(stream(current_output),
			[ graph(Graph),
			  base(Graph)
			]).
send_graph(Graph, canonical_turtle) :- !,
	rdf_save_canonical_turtle(stream(current_output), [graph(Graph)]).
send_graph(Graph, rdfxml) :- !,
	rdf_save(stream(current_output), [graph(Graph)]).

default_mime_type(turtle, text/turtle).
default_mime_type(canonical_turtle, text/turtle).
default_mime_type(rdfxml, application/'rdf+xml').

%%	export_graph_schema(+Request)
%
%	HTTP handler that computes the schema from the actual data in a
%	graph.
%
%	@see The computation is implemented by rdf_graph_schema/2.

export_graph_schema(Request) :-
	http_parameters(Request,
			[ graph(Graph),
			  format(Format),
			  mimetype(Mime)
			],
			[ attribute_declarations(http_param)
			]
		       ),
	authorized(read(default, download(Graph))),
	rdf_graph_schema(Graph, Triples),
	(   Mime == default
	->  default_mime_type(Format, MimeType)
	;   MimeType = Mime
	),
	write_graph(Triples,
		    [ serialization(Format),
		      mimetype(MimeType)
		    ]).


%%	http_param(?Name, ?Attributes).

http_param(graph,
	   [ description('Name of the graph')]).
http_param(format,
	   [ oneof([turtle,
		    canonical_turtle,
		    rdfxml
		   ]),
	     default(turtle),
	     description('Output serialization')
	   ]).
http_param(mimetype,
	   [ default(default),
	     description('MIME-type to use. If "default", it depends on format')
	   ]).



