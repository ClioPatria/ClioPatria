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

:- module(sparql_xml_result,
	  [ sparql_read_xml_result/2,	% +File, -Result
	    sparql_write_xml_result/3	% +Stream, +Result
	  ]).
:- use_module(library(sgml)).
:- use_module(library(assoc)).
:- use_module(library(option)).
:- use_module(library('semweb/rdf_db'), [rdf_is_bnode/1]).

ns(sparql, 'http://www.w3.org/2005/sparql-results#').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Read/write   the   SPARQL   XML   result     format    as   defined   in
http://www.w3.org/TR/rdf-sparql-XMLres/, version 6 April 2006.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	  MACRO HANDLING	*
		 *******************************/

%	substitute 'sparql' by the namespace   defined  above for better
%	readability of the remainder of the code.

term_subst(V, _, _, V) :-
	var(V), !.
term_subst(F, F, T, T) :- !.
term_subst(C, F, T, C2) :-
	compound(C), !,
	functor(C, Name, Arity),
	functor(C2, Name, Arity),
	term_subst(0, Arity, C, F, T, C2).
term_subst(T, _, _, T).

term_subst(A, A, _, _, _, _) :- !.
term_subst(I0, Arity, C0, F, T, C) :-
	I is I0 + 1,
	arg(I, C0, A0),
	term_subst(A0, F, T, A),
	arg(I, C, A),
	term_subst(I, Arity, C0, F, T, C).

term_expansion(T0, T) :-
	ns(sparql, NS),
	term_subst(T0, sparql, NS, T).


		 /*******************************
		 *	     READING		*
		 *******************************/

%%	sparql_read_xml_result(+Input, -Result)
%	
%	Specs from http://www.w3.org/TR/rdf-sparql-XMLres/.  The returned
%	Result term is of the format:
%	
%		* select(VarNames, Rows)
%		Where VarNames is a list of atoms denoting the variable
%		names and Rows is a list of row(....) containg the column
%		values in the same order as the variable names.
%		
%		* ask(Bool)
%		Where Bool is either true or false

sparql_read_xml_result(Input, Result) :-
	load_structure(Input, DOM,
		       [ dialect(xmlns),
			 space(remove)
		       ]),
	dom_to_result(DOM, Result).

dom_to_result(DOM, Result) :-
	sub_element(DOM, sparql:head, _HAtt, Content),
	variables(Content, Vars),
	(   Vars == [],
	    sub_element(DOM, sparql:boolean, _, [TrueFalse])
	->  Result = ask(TrueFalse)
	;   Result = select(Vars, Rows),
	    sub_element(DOM, sparql:results, _RAtt, RContent),
	    rows(RContent, Vars, Rows)
	).

%%	variables(+DOM, -Varnames)
%	
%	Deals with <variable name=Name>.  Head   also  may contain <link
%	href="..."/>. This points to additional   meta-data.  Not really
%	clear what we can do with that.

variables([], []).
variables([element(sparql:variable, Att, [])|T0], [Name|T]) :-
	memberchk(name=Name, Att),
	variables(T0, T).
variables([element(sparql:link, _, _)|T0], T) :-
	variables(T0, T).


rows([], _, []).
rows([R|T0], Vars, [Row|T]) :-
	row_values(Vars, R, Values),
	Row =.. [row|Values],
	rows(T0, Vars, T).

row_values([], _, []).
row_values([Var|VarT], DOM, [Value|ValueT]) :-
	(   sub_element(DOM, sparql:binding, Att, Content),
	    memberchk(name=Var, Att)
	->  value(Content, Value)
	;   Value = '$null$'
	),
	row_values(VarT, DOM, ValueT).
	
value([element(sparql:literal, Att, Content)], literal(Lit)) :-
	lit_value(Content, Value),
	(   memberchk(datatype=Type, Att)
	->  Lit = type(Type, Value)
	;   memberchk(xml:lang=Lang, Att)
	->  Lit = lang(Lang, Value)
	;   Lit = Value
	).
value([element(sparql:uri, [], [URI])], URI).
value([element(sparql:bnode, [], [NodeID])], URI) :-
	atom_concat('__', NodeID, URI).			% DUBIOUS
value([element(sparql:unbound, [], [])], '$null$').


lit_value([], '').
lit_value([Value], Value).


%%	sub_element(+DOM, +Name, -Atttribs, -Content)

sub_element(element(Name, Att, Content), Name, Att, Content).
sub_element(element(_, _, List), Name, Att, Content) :-
	sub_element(List, Name, Att, Content).
sub_element([H|T], Name, Att, Content) :-
	(   sub_element(H, Name, Att, Content)
	;   sub_element(T, Name, Att, Content)
	).


		 /*******************************
		 *	      WRITING		*
		 *******************************/

%%	sparql_write_xml_result(+Out, +Term, +Options)
%	
%	Write SPARQL XML result data.

sparql_write_xml_result(Out, ask(TrueFalse), _Options) :- !,
	write_header(Out),
	format(Out, '  <head/>~n', []),
	format(Out, '  <boolean>~w</boolean>~n', [TrueFalse]),
	format(Out, '</sparql>~n', []).
sparql_write_xml_result(Out, select(VarNames, Rows), Options) :-
	option(ordered(Ordered), Options, false),
	option(distinct(Distinct), Options, false),
	write_header(Out),
	format(Out, '  <head>~n', []),
	write_varnames(VarNames, Out),
	format(Out, '  </head>~n', []),
	format(Out, '  <results ordered="~w" distinct="~w">~n',
	       [Ordered, Distinct]),
	write_rows(Rows, VarNames, Out),
	format(Out, '  </results>~n', []),
	format(Out, '</sparql>~n', []).

write_header(Out) :-
	xml_encoding(Out, Encoding),
	format(Out, '<?xml version="1.0" encoding="~w"?>~n', [Encoding]),
	format(Out, '<sparql xmlns="~w">~n', [sparql]).

xml_encoding(Out, Encoding) :-
	stream_property(Out, encoding(Enc)),
	(   xml_encoding_name(Enc, Encoding)
	->  true
	;   throw(error(domain_error(rdf_encoding, Enc), _))
	).

xml_encoding_name(ascii,       'US-ASCII').
xml_encoding_name(iso_latin_1, 'ISO-8859-1').
xml_encoding_name(utf8,        'UTF-8').

write_varnames([], _).
write_varnames([H|T], Out) :-
	stream_property(Out, encoding(Encoding)),
	xml_quote_attribute(H, Q, Encoding),
	format(Out, '    <variable name="~w"/>~n', [Q]),
	write_varnames(T, Out).

write_rows(Rows, VarNames, Out) :-
	empty_assoc(BNodes),
	write_rows(Rows, VarNames, Out, state(0, BNodes), _).

write_rows([], _, _, State, State).
write_rows([H|T], VarNames, Out, State0, State) :-
	write_row(H, VarNames, Out, State0, State1),
	write_rows(T, VarNames, Out, State1, State).

write_row(Row, VarNames, Out, State0, State) :-
	format(Out, '    <result>~n', []),
	write_bindings(VarNames, 1, Row, Out, State0, State),
	format(Out, '    </result>~n', []).

write_bindings([], _, _, _, State, State).
write_bindings([Name|T], I, Row, Out, State0, State) :-
	arg(I, Row, Value),
	write_binding(Value, Name, Out, State0, State1),
	I2 is I + 1,
	write_bindings(T, I2, Row, Out, State1, State).

write_binding(Var, _, _, S, S) :- var(Var), !.
write_binding('$null$', _, _, S, S) :- !.
write_binding(Value, Name, Out, State0, State) :-
	stream_property(Out, encoding(Encoding)),
	xml_quote_attribute(Name, Q, Encoding),
	format(Out, '      <binding name="~w">~n', [Q]),
	write_binding_value(Value, Out, State0, State),
	format(Out, '      </binding>~n', []).

write_binding_value(literal(Lit), Out, State, State) :-
	write_binding_literal(Lit, Out).
write_binding_value(URI, Out, State0, State) :-
	rdf_is_bnode(URI),
	bnode_id(URI, Id, State0, State),
	format(Out, '        <bnode>~w</bnode>~n', [Id]).
write_binding_value(URI, Out, State, State) :-
	stream_property(Out, encoding(Encoding)),
	xml_quote_cdata(URI, Q, Encoding),
	format(Out, '        <uri>~w</uri>~n', [Q]).

write_binding_literal(type(Type, Value), Out) :- !,
	stream_property(Out, encoding(Encoding)),
	xml_quote_attribute(Type, QT, Encoding),
	xml_quote_cdata(Value, QV, Encoding),
	format(Out, '        <literal datatype="~w">~w</literal>~n', [QT, QV]).
write_binding_literal(lang(L, Value), Out) :- !,
	stream_property(Out, encoding(Encoding)),
	xml_quote_cdata(Value, QV, Encoding),
	format(Out, '        <literal xml:lang="~w">~w</literal>~n', [L, QV]).
write_binding_literal(Value, Out) :- !,
	stream_property(Out, encoding(Encoding)),
	xml_quote_cdata(Value, QV, Encoding),
	format(Out, '        <literal>~w</literal>~n', [QV]).

bnode_id(URI, Id, State0, State) :-
	State0 = state(N, Assoc),
	(   get_assoc(URI, Assoc, Id)
	->  State = State0
	;   N2 is N + 1,
	    Id = N2,			% number 1, ...
	    put_assoc(URI, Assoc, Id, Assoc2),
	    State = state(N2, Assoc2)
	).
