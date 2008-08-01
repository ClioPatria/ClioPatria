/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2004, University of Amsterdam

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

:- module(xml_result,
	  [ xml_write_result_table/3,	% +Out, +Rows, +Options
	    xml_read_result_table/3,	% +In, -Rows, -VarNames
	    xml_to_result_table/3	% +XML, -Rows, -VarNames
	  ]).
:- use_module(library(assoc)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library(sgml)).

:- multifile
	rdf_io:write_table/4.


		 /*******************************
		 *	      WRITING		*
		 *******************************/

%%	write_table(+Format, +Serialization, +Rows, +Options)
%	
%	Write a result table in Sesame compliant XML format.
%	
%	@param Format Must be =xml=

rdf_io:write_table(xml, _, Rows, Options) :-
	format('Transfer-encoding: chunked~n'),
	format('Content-type: text/xml; charset=UTF-8~n~n'),
	xml_write_result_table(current_output, Rows, Options).

xml_write_result_table(Out, Rows, Options) :-
	format(Out, '<?xml version="1.0" encoding="UTF-8"?>~n~n', []),
	format(Out, '<tableQueryResult>~n', []),
	header(Out, Options),
	tuples(Out, Rows),
	format(Out, '</tableQueryResult>~n', []).

%%	header(+Out, +Options) is det.
%
%	Write  the  column-names  obtained   from  the  variables(+Vars)
%	option. Vars is either a list  of   atoms  or a term holding the
%	comlumn names as arguments (as in v('Name', 'Age')).

header(Out, Options) :-
	memberchk(variables(Vars), Options), !,
	(   is_list(Vars)
	->  Names = Vars
	;   Vars =.. [_|Names]
	),
	format(Out, '  <header>~n', []),
	column_names(Names, Out),
	format(Out, '  </header>~n', []).
header(_, _).

column_names([], _).
column_names([H|T], Out) :-
	format(Out, '    <columnName>~w</columnName>~n', [H]),
	column_names(T, Out).

tuples(Out, Rows) :-
	empty_assoc(Map),		% URL --> BnodeID
	tuples(Rows, Out, 1, Map).

tuples([], _, _, _).
tuples([H|T], Out, BN0, Map0) :-
	H =.. [_|Columns],
	format(Out, '  <tuple>~n', []),
	columns(Columns, Out, BN0, BN, Map0, Map),
	format(Out, '  </tuple>~n', []),
	tuples(T, Out, BN, Map).

columns([], _, BN, BN, Map, Map).
columns([H|T], Out, BN0, BN, Map0, Map) :-
	column(H, Out, BN0, BN1, Map0, Map1),
	columns(T, Out, BN1, BN, Map1, Map).

column(Var, Out, BN, BN, Map, Map) :-
	var(Var), !,
	format(Out, '    <null/>~n', []).
column('$null$', Out, BN, BN, Map, Map) :- !,
	format(Out, '    <null/>~n', []).
column(literal(L), Out, BN, BN, Map, Map) :- !,
	literal(L, Out).
column(Anon, Out, BN, BN, Map, Map) :-
	get_assoc(Anon, Map, BNode), !,
	format(Out, '    <bNode>~w</bNode>~n', [BNode]).
column(Anon, Out, BN0, BN, Map0, Map) :-
	rdf_is_bnode(Anon), !,
	BN is BN0 + 1,
	atom_concat(node, BN, BNode),
	format(Out, '    <bNode>~w</bNode>~n', [BNode]),
	put_assoc(Anon, Map0, BNode, Map).
column(URI, Out, BN, BN, Map, Map) :-
	xml_quote_cdata(URI, QURI, utf8),
	format(Out, '    <uri>~w</uri>~n', [QURI]).

literal(type(Type, String), Out) :- !,
	xml_quote_cdata(String, QString, utf8),
	format(Out, '    <literal dataType="~w">~w</literal>~n',
	       [Type, QString]).
literal(lang(Lang, String), Out) :- !,
	xml_quote_cdata(String, QString, utf8),
	format(Out, '    <literal xml:lang="~w">~w</literal>~n', [Lang, QString]).
literal(String, Out) :- !,
	xml_quote_cdata(String, QString, utf8),
	format(Out, '    <literal>~w</literal>~n', [QString]).


		 /*******************************
		 *	      READING		*
		 *******************************/

%%	xml_read_result_table(+In, -Rows, -VarNames)
%	
%	Read an XML document from In and   return  the rows and variable
%	names in there.

xml_read_result_table(In, Rows, VarNames) :-
	load_structure(stream(In), XML,
		       [ dialect(xml),
			 space(remove)
		       ]),
	xml_to_result_table(XML, Rows, VarNames).

%%	xml_to_result_table(+XML, -Rows, -VarNames)
%	
%	Convert a parsed XML document into a   list of rows and a column
%	name (variable name) term of the format names(Col1, Col2, ...).


xml_to_result_table([XML], Rows, VarNames) :- !,
	xml_to_result_table(XML, Rows, VarNames).
xml_to_result_table(element(tableQueryResult, _, Content), Rows, VarNames) :-
	phrase(result_table(Rows, VarNames), Content).

result_table(Rows, VarNames) -->
	result_header(VarNames),
	result_rows(Rows).

result_header(VarNames) -->
	[ element(header, _, Content)
	], !,
	{ phrase(column_names(Columns), Content),
	  VarNames =.. [names|Columns]
	}.
result_header(names) -->
	[].

column_names([]) -->
	[].
column_names([Name|T]) -->
	[ element(columnName, _, [Name])
	],
	column_names(T).

result_rows([Row|Rows]) -->
	[ element(tuple, _, Content)
	],
	{ phrase(columns(Columns), Content),
	  Row =.. [row|Columns]
	},
	result_rows(Rows).
result_rows([]) -->
	[].

columns([H|T]) -->
	column(H), !,
	columns(T).
columns([]) -->
	[].

column(URI) -->
	[ element(uri, _, [URI])
	], !.
column(Bnode) -->
	[ element(bNode, _, [Bnode])
	], !.
column(literal(Literal)) -->
	[ element(literal, A, [String]) ],
	{   memberchk(datatype=Type, A)
	->  Literal = type(Type, String)
	;   memberchk('xml:lang'=Lang, A)
	->  Literal = lang(Lang, String)
	;   Literal = String
	}.
column('$null$') -->
	[ element(null, _, [])
	], !.
