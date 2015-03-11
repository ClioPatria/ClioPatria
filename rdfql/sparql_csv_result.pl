/*  Part of ClioPatria

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013-2015, VU University Amsterdam

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

:- module(sparql_csv_result,
	  [ sparql_write_csv_result/3	% +Out, +Result, +Options
	  ]).
:- use_module(library(csv)).
:- use_module(library(assoc)).
:- use_module(library(sgml_write)).
:- use_module(library(semweb/rdf_db)).

/** <module> Write SPARQL results as CSV

@see	http://www.w3.org/TR/2013/REC-sparql11-results-csv-tsv-20130321/
*/

sparql_csv_mime_type('text/tab-separated-values; charset=UTF-8').

%%	sparql_write_json_result(+Out:stream, +Result, +Options) is det.
%
%	Emit results from a SPARQL SELECT query as CSV.
%
%	@see http://www.w3.org/TR/rdf-sparql-json-res/

sparql_write_csv_result(Out, select(VarTerm, Rows), _Options) :- !,
	empty_assoc(BNodeDict),
	rows_to_csv(Rows, CSVRows, bnode(1, BNodeDict), _),
	sparql_csv_mime_type(ContentType),
	format('Content-type: ~w~n~n', [ContentType]),
	csv_write_stream(Out, [VarTerm|CSVRows], []).
sparql_write_csv_result(_Out, Result, _Options) :- !,
	domain_error(csv_sparql_result, Result).

rows_to_csv([], [], BNodeDict, BNodeDict).
rows_to_csv([H0|T0], [H|T], BNodeDict0, BNodeDict) :-
	row_to_csv(H0, H, BNodeDict0, BNodeDict1),
	rows_to_csv(T0, T, BNodeDict1, BNodeDict).

row_to_csv(RDF, CSV, BNodeDict0, BNodeDict) :-
	RDF =.. [_|RDFFields],
	fields_to_csv(RDFFields, CSVFields, BNodeDict0, BNodeDict),
	CSV =.. [row|CSVFields].

fields_to_csv([], [], BNodeDict, BNodeDict).
fields_to_csv([H0|T0], [H|T], BNodeDict0, BNodeDict) :-
	field_to_csv(H0, H, BNodeDict0, BNodeDict1),
	fields_to_csv(T0, T, BNodeDict1, BNodeDict).

field_to_csv(Var, '', BNodeDict, BNodeDict) :-
	(   var(Var)
	->  true
	;   Var == '$null$'
	), !.
field_to_csv(literal(Literal), Text, BNodeDict, BNodeDict) :-
	literal_text(Literal, Text), !.
field_to_csv(Resource, BNode, BNodeDict0, BNodeDict) :-
	rdf_is_bnode(Resource), !,
	BNodeDict0 = bnode(N0, Dict0),
	(   get_assoc(Resource, Dict0, BNode)
	->  BNodeDict = BNodeDict0
	;   succ(N0, N),
	    atomic_list_concat(['_:node', N], BNode),
	    put_assoc(Resource, Dict0, BNode, Dict),
	    BNodeDict = bnode(N, Dict)
	).
field_to_csv(Atomic, Atomic, BNodeDict, BNodeDict) :-
	atomic(Atomic), !.
field_to_csv(Term, String, BNodeDict, BNodeDict) :-
	term_string(Term, String).

%%	literal_text(+Literal, -Value) is semidet.

literal_text(type(Type, Value), Text) :- !,
	atom(Type),
	(   rdf_equal(Type, rdf:'XMLLiteral')
	->  with_output_to(string(Text),
			   xml_write(Value, [header(false)]))
	;   atomic(Value)
	->  Text = Value
	;   term_string(Value, Text)
	).
literal_text(lang(Lang, LangText), Text) :- !,
	atom(Lang),
	literal_text(LangText, Text).
literal_text(Text, Text) :-
	atom(Text), !.
literal_text(Text, Text) :-
	string(Text), !.


		 /*******************************
		 *   INTERACTIVE QUERY RESULT	*
		 *******************************/

:- multifile
	rdf_io:write_table/4.

rdf_io:write_table(csv, _, Rows, Options) :-
	memberchk(variables(Vars), Options), !,
	(   is_list(Vars)
	->  VarTerm =.. [vars|Vars]
	;   VarTerm = Vars
	),
	sparql_write_csv_result(current_output, select(VarTerm, Rows),
				[ content_type(text/plain),
				  Options
				]).
