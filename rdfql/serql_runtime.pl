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

:- module(serql_runtime,
	  [ serql_compare/3,		% +Comparison, +Left, +Right
	    serql_eval/2,		% +Term, -Evaluated
	    serql_member_statement/2	% -Triple, +List
	  ]).
:- use_module(library(xsdp_types)).
:- use_module(library(debug)).
:- use_module(library('semweb/rdf_db')).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module provides runtime support for running compiled SeRQL queries.
I.e. it defines special constructs that  may   be  emitted  by the SeRQL
compiler and optimizer. Predicates common to   all  query languages have
been moved to rdfql_runtime.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	   RUNTIME TESTS	*
		 *******************************/

%%	serql_compare(+Op, +Left, +Right)
%	
%	Handle numerical and textual comparison of literals.  Some work
%	must be done at compiletime.

serql_compare(Op, L, R) :-
	serql_eval(L, VL),
	serql_eval(R, VR), !,
	do_compare(Op, VL, VR).

do_compare(like, literal(Value), Pattern) :- !,
	to_string(Value, String),
	rdf_match_label(like, Pattern, String).
do_compare(like, Resource, Pattern) :- !,
	atom(Resource),
	rdf_match_label(like, Pattern, Resource).
do_compare(=, X, X) :- !.
do_compare(=, literal(X), query(X)) :- !.
do_compare(=, X, query(X)) :- !.
do_compare(\=, X, Y) :- !,
	\+ do_compare(=, X, Y).
do_compare(Op, literal(Data), query(Query)) :-
	catch(to_number(Query, Right, TypeQ), _, fail), !,
	(   nonvar(TypeQ), atom(Data)
	->  catch(xsdp_convert(TypeQ, [Data], Left), _, fail)
	;   catch(to_number(Data, Left, TypeD), _, fail),
	    serql_subsumes(TypeQ, TypeD)
	),
	cmp_nums(Op, Left, Right).
do_compare(Op, literal(Data), query(Query)) :- !,
	(   atom(Query)			% plain text
	->  atom(Data),			% TBD: Lang and Type
	    cmp_strings(Op, Data, Query)
	).
do_compare(Op, query(Query), literal(Data)) :- !,
	inverse_op(Op, Inverse),
	do_compare(Inverse, literal(Data), query(Query)).
do_compare(Op, literal(Value), literal(Number)) :-
	catch(to_number(Value, Left, TypeL), _, fail),
	catch(to_number(Number, Right, TypeR), _, fail),
	TypeL == TypeR,
	cmp_nums(Op, Left, Right).

serql_eval(Var, X) :-
	var(Var), !,
	X = '$null$'.
serql_eval(lang(X), Lang) :- !,
	lang(X, Lang).
serql_eval(datatype(X), Type) :- !,
	datatype(X, Type).
serql_eval(label(X), Lang) :- !,
	label(X, Lang).
serql_eval(X, X).

%%	lang(+Literal, -Lang) is det.
%%	datatype(+Literal, -DataType) is det.
%%	label(+Literal, -Label) is det.
%	
%	Defined functions on literals.  

lang(literal(lang(Lang0, _)), Lang) :-
     nonvar(Lang0), !,
     Lang = literal(Lang0).
lang(_, '$null$').

datatype(literal(type(Type0, _)), Type) :-
     nonvar(Type0), !,
     Type = Type0.
datatype(_, '$null$').

label(literal(lang(_, Label0)), Label) :-
     nonvar(Label0), !,
     Label = literal(Label0).
label(literal(Label0), Label) :-
     nonvar(Label0), !,
     Label = literal(Label0).
label(_, '$null$').


cmp_nums(=, L, R)  :- L =:= R.
cmp_nums(=<, L, R) :- L =< R.
cmp_nums(<, L, R)  :- L < R.
cmp_nums(>, L, R)  :- L > R.
cmp_nums(>=, L, R) :- L >= R.

cmp_strings(Op, S1, S2) :-
	atom(S1), atom(S2),
	compare(Op, S1, S2).

inverse_op(=, =).
inverse_op(=<, >).
inverse_op(<, >=).
inverse_op(>, =<).
inverse_op(>=, <).

to_number(type(Type, String), Num, Type) :- !,	% TBD: Check type
	atom_number(String, Num).
to_number(lang(_Lang, String), Num, _) :- !,
	atom_number(String, Num).
to_number(String, Num, _) :-
	assertion(atom(String)),
	atom_number(String, Num).

%%	serql_subsumes(QueryType, DataType)

serql_subsumes(Q, Var) :-		% odd rule!
	nonvar(Q),
	var(Var), !.
serql_subsumes(_, Var) :-		% odd rule!
	var(Var), !, fail.
serql_subsumes(Var, _) :-		% type is not specified in query
	var(Var), !.
serql_subsumes(Query, Data) :-
	xsdp_subtype_of(Data, Query).

to_string(lang(_, String), String) :- !.
to_string(type(_, String), String) :- !.
to_string(String, String) :-
	atom(String).

%%	serql_member_statement(-Triple, +List)
%	
%	Get the individual triples from  the   original  reply. Used for
%	CONSTRUCT queries. As handling optional matches is different int
%	SeRQL compared to SPARQL, the selection  is in the SeRQL runtime
%	module.

serql_member_statement(RDF, [H|_]) :-
	member_statement2(RDF, H).
serql_member_statement(RDF, [_|T]) :-
	serql_member_statement(RDF, T).

member_statement2(RDF, optional(True, Statements)) :- !,
	True = true,
	serql_member_statement(RDF, Statements).
member_statement2(RDF, RDF).
