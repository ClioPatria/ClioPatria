/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, University of Amsterdam

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

:- module(sparql_json_result,
	  [ sparql_write_json_result/3	% +Out, +Result, +Options
	  ]).
:- use_module(library(http/http_json)).
:- use_module(library(sgml_write)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).

/** <module> Write SPARQL results as JSON

@tbd	Support other SPARQL request results
@author Jan Wielemaker
@author Michiel Hildebrand
*/

sparql_json_mime_type(application/'sparql-results; charset=UTF-8').

%%	sparql_write_json_result(+Out:stream, +Result, +Options) is det.
%
%	Emit results from a SPARQL query as JSON.
%
%	@see http://www.w3.org/TR/rdf-sparql-json-res/

sparql_write_json_result(Out, select(VarTerm, Rows), Options) :-
	VarTerm =.. [_|VarNames],
	JSON = json([ head    = json([vars=VarNames]),
		      results = json([bindings=Bindings])
		    ]),
	maplist(row_to_json(VarNames), Rows, Bindings),
	(   option(content_type(_), Options)
	->  JSONOptions = Options
	;   sparql_json_mime_type(Mime),
	    JSONOptions = [content_type(Mime)|Options]
	),
	with_output_to(Out, reply_json(JSON, JSONOptions)).
sparql_write_json_result(Out, ask(True), Options) :-
	JSON = json([ head    = json([]),
		      boolean = @(True)
		    ]),
	(   option(content_type(_), Options)
	->  JSONOptions = Options
	;   sparql_json_mime_type(Mime),
	    JSONOptions = [content_type(Mime)|Options]
	),
	with_output_to(Out, reply_json(JSON, JSONOptions)).


row_to_json(Vars, Row, json(Bindings)) :-
	var_col_bindings(Vars, 1, Row, Bindings).

var_col_bindings([], _, _, []).
var_col_bindings([V0|T0], I, Row, [V0=json(JSON)|T]) :-
	arg(I, Row, Value),
	rdf_term_to_json(Value, JSON),
	I2 is I + 1,
	var_col_bindings(T0, I2, Row, T).


%%	rdf_term_to_json(+RDFTerm, -JsonTerm)
%
%	convert an rdf term to a json term.

rdf_term_to_json(literal(Lit), Object) :- !,
	Object = [value=Txt, type=literal|Rest],
	literal_to_json(Lit, Txt, Rest).
rdf_term_to_json(URI0, Object) :-
	rdf_global_id(URI0, URI),
	Object = [value=URI, type=Type],
	object_uri_type(URI, Type).

%%	literal_to_json(+Literal, -Text, -Attributes)
%
%	Extract text and Attributes from Literal resource.

literal_to_json(lang(Lang, Text), Text, [lang=Lang]) :- !.
literal_to_json(type(Type, Text0), Text, [datatype=Type]) :- !,
	to_text(Type, Text0, Text).
literal_to_json(Txt, Txt, []).

to_text(_Type, Text, Text) :-
	atomic(Text).
to_text(Type, DOM, Text) :-
	rdf_equal(Type, rdf:'XMLLiteral'), !,
	with_output_to(string(Text),
		       xml_write(DOM, [header(false)])),
	atomic(Text).

%%	object_uri_type(+URI, -Type)
%
%	Type is one of bnode or uri.

object_uri_type(URI, Type) :-
	(   rdf_is_bnode(URI)
	->  Type = bnode
	;   Type = uri
	).

		 /*******************************
		 *   INTERACTIVE QUERY RESULT	*
		 *******************************/

:- multifile
	rdf_io:write_table/4.

rdf_io:write_table(json, _, Rows, Options) :-
	memberchk(variables(Vars), Options), !,
	(   is_list(Vars)
	->  VarTerm =.. [vars|Vars]
	;   VarTerm = Vars
	),
	sparql_write_json_result(current_output, select(VarTerm, Rows),
				 [ content_type(text/plain),
				   Options
				 ]).
