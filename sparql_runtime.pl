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

:- module(sparql_runtime,
	  [ sparql_true/1,
	    sparql_eval/2
	  ]).
:- use_module(library('semweb/rdf_db')).
:- use_module(library(xsdp_types)).

:- discontiguous
	term_expansion/2.

%%	sparql_true(+Term)
%
%	Generated from FILTER Term, where Term must be converted to a
%	boolean as 'Effective Boolean Value'.

sparql_true(Term) :-
	typed_eval(boolean, Term, Result), !,
	true(Result).

true(boolean(true)).

%%	eval(+Term, -Result)

eval(Var, unbound(Var)) :-
	var(Var), !.
eval(literal(Literal), Result) :- !,
	eval_literal(Literal, Result).
eval(Atom, iri(Atom)) :-
	atom(Atom), !.
eval(Term, Result) :-
	sparql_op(Term, Types), !,
	Term =.. [Op|Args0],
	eval_args(Args0, Types, Args),
	EvalTerm =.. [Op|Args],
	op(EvalTerm, Result).
eval(function(Term), Result) :- !,
	(   xsd_cast(Term, Type, Value)
	->  eval_cast(Type, Value, Result)
	;   eval_function(Term, Result)
	).
eval(Term, Term).			% Result of sub-eval

eval_args([], [], []).
eval_args([H0|T0], [Type0|Types], [H|T]) :-
	(   typed_eval(Type0, H0, H)
	->  true
	;   H = boolean(error)
	),
	eval_args(T0, Types, T).

%%	eval(+Type, +Term, -Result)
%
%	Evaluate Term, converting the resulting argument to Type.

typed_eval(no_eval, Term, Term).
typed_eval(any, Term, Result) :-
	eval(Term, Result).
typed_eval(simple_literal, Term, Result) :-
	eval(Term, Result).
typed_eval(boolean, Term, Result) :-
	eval(Term, Result0),
	effective_boolean_value(Result0, Result).
typed_eval(numeric, Term, Result) :-
	eval(Term, Eval),
	(   Eval = numeric(_,_)
	->  Result = Eval
	;   throw(error(type_error(numeric, Result), _))
	).


eval_literal(type(Type, Atom), Value) :- !,
	eval_typed_literal(Type, Atom, Value).
eval_literal(lang(Lang, Atom), lang(Lang, Atom)) :- !.
eval_literal(Atom, simple_literal(Atom)) :-
	atom(Atom), !.

eval_typed_literal(Type, Atom, numeric(Type, Value)) :-
	xsdp_numeric_uri(Type, Generic), !,
	numeric_literal_value(Generic, Atom, Value).
eval_typed_literal(Type, Atom, boolean(Atom)) :-
	rdf_equal(Type, xsd:boolean), !.
eval_typed_literal(Type, Atom, string(Atom)) :-
	rdf_equal(Type, xsd:string), !.
eval_typed_literal(Type, Atom, date_time(Atom)) :-
	rdf_equal(Type, xsd:dateTime), !.
eval_typed_literal(Type, Atom, typed_literal(Type, Atom)).

%%	numeric_literal_value(+Literal, -Value) is semidet.
%
%	Convert a SPARQL numeric literal into  its value for the purpose
%	of comparison-by-value.
%
%	@tbd	Move this into the rdf_db library.  There we can achieve
%		better performance and we can do more efficient
%		matching.

numeric_literal_value(Type, Text, Value) :-
	rdf_equal(Type, xsd:integer), !,
	catch(atom_number(Text, Value), _, fail),
	integer(Value).
numeric_literal_value(Type, Text, Value) :-
	rdf_equal(Type, xsd:decimal), !,
	catch(atom_number(Text, Value), _, fail).
numeric_literal_value(_, Text, Value) :-
	catch(atom_number(Text, Value), _, fail), !.
numeric_literal_value(_, Text, Value) :-
	catch(rdf_text_to_float(Text, Value), _, fail).

rdf_text_to_float(Text, Value) :-
	atom_codes(Text, Codes),
	optional_sign(Codes, Rest, Sign),
	(   Rest = [0'.|_]
	->  number_codes(NonnegValue, [0'0|Rest])
	;   last(Rest, 0'.)
	->  append(Rest, [0'0], NonnegCodes),
	    number_codes(NonnegCodes, NonnegValue)
        ),
	Value is NonnegValue*Sign.

optional_sign([0'+|Rest], Rest, 1) :- !.
optional_sign([0'-|Rest], Rest, -1) :- !.
optional_sign(Rest, Rest, 1).

%%	op(+Operator, -Result) is semidet.
%
%	@param Operator	Term of the format Op(Arg...) where each Arg
%			is embedded in its type.
%	@param Result	Result-value, embedded in its type.

% SPARQL Unary operators
op(not(boolean(X)), boolean(Result)) :-
	not(X, Result).
op(+(numeric(Type, X)), numeric(Type, X)).
op(-(numeric(Type, X)), numeric(Type, Result)) :-
	Result is -X.

% SPARQL Tests, defined in section 11.4
op(bound(X), boolean(Result)) :-
	(bound(X) -> Result = true ; Result = false).
op(isiri(X), boolean(Result)) :-
	(isiri(X) -> Result = true ; Result = false).
op(isuri(X), boolean(Result)) :-
	(isiri(X) -> Result = true ; Result = false).
op(isblank(X), boolean(Result)) :-
	(isblank(X) -> Result = true ; Result = false).
op(isliteral(X), boolean(Result)) :-
	(isliteral(X) -> Result = true ; Result = false).

% SPARQL Accessors
op(str(X), simple_literal(Str)) :-
	str(X, Str).
op(lang(X), simple_literal(Lang)) :-
	lang(X, Lang).
op(datatype(X), Type) :-
	datatype(X, Type).

% SPARQL Binary operators
% Logical connectives, defined in section 11.4
op(and(boolean(A), boolean(B)), boolean(Result)) :-
	sparql_and(A, B, Result).
op(or(boolean(A), boolean(B)), boolean(Result)) :-
	sparql_or(A, B, Result).

% XPath Tests
op(numeric(_, X) = numeric(_, Y), boolean(Result)) :-
	(X =:= Y -> Result = true ; Result = false).
op(date_time(X) = date_time(Y), boolean(Result)) :-
	(X == Y -> Result = true ; Result = false).
op(numeric(_, X) \= numeric(_, Y), boolean(Result)) :-
	(X =\= Y -> Result = true ; Result = false).
op(date_time(X) \= date_time(Y), boolean(Result)) :-
	(X \== Y -> Result = true ; Result = false).
%<
op(numeric(_, X) < numeric(_, Y), boolean(Result)) :-
	(X < Y -> Result = true ; Result = false).
op(simple_literal(X) < simple_literal(Y), boolean(Result)) :-
	(X @< Y -> Result = true ; Result = false).
op(string(X) < string(Y), boolean(Result)) :-
	(X @< Y -> Result = true ; Result = false).
op(date_time(X) < date_time(Y), boolean(Result)) :-
	(X @< Y -> Result = true ; Result = false).
%>
op(numeric(_, X) > numeric(_, Y), boolean(Result)) :-
	(X > Y -> Result = true ; Result = false).
op(simple_literal(X) > simple_literal(Y), boolean(Result)) :-
	(X @> Y -> Result = true ; Result = false).
op(string(X) > string(Y), boolean(Result)) :-
	(X @> Y -> Result = true ; Result = false).
op(date_time(X) > date_time(Y), boolean(Result)) :-
	(X @> Y -> Result = true ; Result = false).
%=<
op(numeric(_, X) =< numeric(_, Y), boolean(Result)) :-
	(X =< Y -> Result = true ; Result = false).
op(simple_literal(X) =< simple_literal(Y), boolean(Result)) :-
	(X @=< Y -> Result = true ; Result = false).
op(string(X) =< string(Y), boolean(Result)) :-
	(X @=< Y -> Result = true ; Result = false).
op(date_time(X) =< date_time(Y), boolean(Result)) :-
	(X @=< Y -> Result = true ; Result = false).
%>=
op(numeric(_, X) >= numeric(_, Y), boolean(Result)) :-
	(X >= Y -> Result = true ; Result = false).
op(simple_literal(X) >= simple_literal(Y), boolean(Result)) :-
	(X @>= Y -> Result = true ; Result = false).
op(string(X) >= string(Y), boolean(Result)) :-
	(X @>= Y -> Result = true ; Result = false).
op(date_time(X) >= date_time(Y), boolean(Result)) :-
	(X @>= Y -> Result = true ; Result = false).

op(numeric(TX, X) * numeric(TY, Y), numeric(Type, Result)) :-
	Result is X * Y,
	combine_types(TX, TY, Type).
op(numeric(TX, X) / numeric(TY, Y), numeric(Type, Result)) :-
	Result is X / Y,
	combine_types_div(TX, TY, Type).
op(numeric(TX, X) + numeric(TY, Y), numeric(Type, Result)) :-
	Result is X + Y,
	combine_types(TX, TY, Type).
op(numeric(TX, X) - numeric(TY, Y), numeric(Type, Result)) :-
	Result is X - Y,
	combine_types(TX, TY, Type).

% SPARQL Tests, defined in section 11.4

op(X = Y, Result) :-
	rdf_equal(X, Y, Result).
op(X \= Y, boolean(Result)) :-
	rdf_equal(X, Y, boolean(R0)),
	not(R0, Result).
op(langmatches(simple_literal(Lang),
	       simple_literal(Pat)),
   boolean(Result)) :-
	(langmatches(Lang, Pat) -> Result = true ; Result = false).
op(regex(simple_literal(Pat),
	 simple_literal(String)),
   boolean(Result)) :-
	(regex(Pat, String, '') -> Result = true ; Result = false).
op(regex(simple_literal(Pat),
	 simple_literal(String),
	 simple_literal(Flags)),
   boolean(Result)) :-
	(regex(Pat, String, Flags) -> Result = true ; Result = false).

%	Numeric types follows the Xpath definitions of
%	http://www.w3.org/TR/xpath-functions/#numeric-functions
%	TBD:

%%	combine_types_div(+TypeLeft, +TypeRight, -Type)

combine_types_div(TX, TY, T) :-
	rdf_equal(xsd:integer, IntType),
	xsdp_numeric_uri(TX, IntType),
	xsdp_numeric_uri(TY, IntType), !,
	rdf_equal(xsd:decimal, T).
combine_types_div(TX, TY, T) :-
	combine_types(TX, TY, T).

%%	combine_types(+TypeLeft, +TypeRight, -Type)

%combine_types(T, T, T) :- !.
combine_types(TL, TR, T) :-
	xsdp_numeric_uri(TL, STL),
	xsdp_numeric_uri(TR, STR),
	promote_types(STL, STR, T).

promote_types(TL, TR, T) :-
	type_index(TL, IL),
	type_index(TR, IR),
	TI is max(IL, IR),
	type_index(T, TI), !.

term_expansion(type_index(NS:Local, I), type_index(URI, I)) :-
	rdf_global_id(NS:Local, URI).

type_index(xsd:integer, 1).
type_index(xsd:decimal, 2).
type_index(xsd:float,   3).
type_index(xsd:double,  4).


%%	rdf_equal(+RDFTerm, +RDFTerm, -Boolean)
%
%	RDF Term equivalence. Described as   lexical equivalence, except
%	where we have the logic to do value equivalence.

rdf_equal(X, X, boolean(true)) :- !.
rdf_equal(boolean(A), boolean(B),  boolean(Eq)) :- !,
	eq_bool(A, B, Eq).
rdf_equal(numeric(_, A), numeric(_, B),  boolean(Eq)) :- !,
	(A =:= B -> Eq = true ; Eq = false).
rdf_equal(_, _, boolean(false)).

eq_bool(X, X, true) :- !.
eq_bool(true, false, false) :- !.
eq_bool(false, true, false) :- !.
eq_bool(X, Y, true) :-
	boolean_value(X, V1),
	boolean_value(Y, V2),
	V1 == V2, !.
eq_bool(_, _, false).

%%	boolean_value(+Content, -Bool)
%
%	Convert the value from literal(xsd:boolean, Content) into
%	either 'true' or 'false'.

boolean_value(true,  true) :- !.
boolean_value(false, false) :- !.
boolean_value('0',   false) :- !.
boolean_value('',    false) :- !.
boolean_value(False, false) :-
	downcase_atom(False, false), !.
boolean_value(_,     true).


:- dynamic
	sparql_op/2.			% +Term, -Types

make_op_declarations :-
	retractall(sparql_op(_,_)),
	findall(Head, clause(op(Head, _), _), Heads0),
	sort(Heads0, Heads),
	make_op_declarations(Heads).

make_op_declarations([]).
make_op_declarations([H0|T0]) :-
	functor(H0, Op, Arity),
	functor(G, Op, Arity),
	same_functor(G, T0, T1, T2),
	make_op_declaration([H0|T1]),
	make_op_declarations(T2).

same_functor(F, [H|T0], [H|T], L) :-
	\+ \+ F = H, !,
	same_functor(F, T0, T, L).
same_functor(_, L, [], L).

make_op_declaration([Op|T]) :-
	functor(Op, Name, Arity),
	functor(G, Name, Arity),
	Op =.. [Name|Args],
	make_types(Args, 1, T, Types),
	assert(sparql_op(G, Types)).

make_types([], _, _, []).
make_types([H0|T0], I, Alt, [H|T]) :-
	alt_types(Alt, I, AT),
	list_to_set([H0|AT], Types),
	make_type(Types, H),
	I2 is I + 1,
	make_types(T0, I2, Alt, T).

alt_types([], _, []).
alt_types([H0|T0], I, [H|T]) :-
	arg(I, H0, H),
	alt_types(T0, I, T).

make_type([T],		       no_eval) :-
	var(T), !.
make_type([boolean(_)],	       boolean) :- !.
make_type([numeric(_, _)],     numeric) :- !.
make_type([simple_literal(_)], simple_literal) :- !.
make_type(_,		       any).

:- make_op_declarations.

		 /*******************************
		 *	       CASTS		*
		 *******************************/

%%	xsd_cast(+Term, -Type, -Arg)
%
%	Deals with xsd:dateTime(?a), casting ?a to   the XML Schema type
%	dateTime. Supported types are the numeric types, xsd:boolean and
%	xsd:dateTime.

term_expansion(xsd_casts, Clauses) :-
	findall(Clause, xsd_cast_clause(Clause), Clauses).

xsd_cast_clause(xsd_cast(Term, Type, Arg)) :-
	(   xsdp_numeric_uri(Type, _)
	;   rdf_equal(xsd:dateTime, Type)
	;   rdf_equal(xsd:boolean, Type)
	),
	Term =.. [Type,Arg].

xsd_casts.

%%	eval_cast(+Type, +Value, -Result)
%
%	Case Value to Type, resulting in a   typed  literal. Can we only
%	case simple literals?

eval_cast(Type, literal(Value), Result) :-
	atom(Value), !,
	eval_typed_literal(Type, Value, Result).


%%	eval_function(+Term, -Result)
%
%	Eval user-defined function.  User-defined functions are of the
%	form sparql:function(Term, Result).

eval_function(Term0, Result) :-
	Term0 =.. [F|Args0],
	eval_args(Args0, Args),
	Term =.. [F|Args],
	sparql:function(Term, Result0), !,
	eval(Result0, Result).
eval_function(Term, boolean(error)) :-
	functor(Term, Name, Arity),
	functor(Gen, Name, Arity),
	clause(sparql:function(Gen, _Result), _Body), !.
eval_function(Term, _) :-
	functor(Term, Name, Arity),
	throw(error(existence_error(sparql_function, Name/Arity), _)).

eval_args([], []).
eval_args([H0|T0], [H|T]) :-
	sparql_eval(H0, H),
	eval_args(T0, T).


		 /*******************************
		 *	SUPPORT PREDICATES	*
		 *******************************/

%%	not(+Bool, -Negated)

not(true, false).
not(false, true).

%%	bound(X)
%
%	Does not evaluate args.  If the argument is a function it
%	is always bound.

bound(X) :- nonvar(X).

%%	str(+RDFTerm, -Atom)
%
%	Extract lexical representation from RDFTerm.

str(Var, _) :-
	var(Var), !, fail.
str(literal(X), Str) :- !,
	str_literal(X, Str).
str(IRI, IRI) :-
	atom(IRI), !,
	\+ rdf_is_bnode(IRI).
str(Expr, Str) :-
	eval(Expr, Value),
	str_value(Value, Str).

str_value(simple_literal(X), X) :- !.
str_value(boolean(X), X) :- !.
str_value(string(X), X) :- !.
str_value(iri(IRI), IRI) :- !.

str_literal(type(_, Str), Str) :- !.
str_literal(lang(_, Str), Str) :- !.
str_literal(Str, Str).

%%	lang(+RDFTerm, -Lang)
%
%	Extract language specification from an RDFTerm

lang(0, _) :- !, fail.			% catch variables.
lang(lang(Lang, _), Lang) :- !.
lang(literal(lang(Lang, _)), Lang) :- !.
lang(literal(_), '').			% Fail on typed?

%%	datatype(+RDFTerm, -IRI)
%
%	Extract type specification from an RDFTerm

datatype(0, _) :- !, fail.
datatype(literal(type(Type, _)), iri(Type)) :- !.
datatype(numeric(Type, _), iri(Type)) :- !.
datatype(boolean(_), iri(Type)) :- !,
	rdf_equal(xsd:boolean, Type).
datatype(Expr, Type) :-
	eval(Expr, Value),
	Value \== Expr,
	datatype(Value, Type).


%%	sparql_and(+A, +B, -Result)

sparql_and(true, true,  true) :- !.
sparql_and(true, error, error) :- !.
sparql_and(error, true, error) :- !.
sparql_and(_,    _,     false).

%%	sparql_or(+A, +B, -Result)

sparql_or(true,	 _,	true) :- !.
sparql_or(_,	 true,	true) :- !.
sparql_or(false, false,	false) :- !.
sparql_or(_,	 _,	error).

%%	langmatches(+Lang, +Pattern)
%
%	Section 11.4.11 function LangMatches.  This   is  slow. Guess we
%	better move this to the  RDF  library.   Note  that  none of the
%	functions return a language qualified   literal and we therefore
%	we only have to consider  the  case   where  the  argument  is a
%	variable also appearing in the object-field of a rdf/3 call.

langmatches('', _) :- !, fail.
langmatches(_, *).
langmatches(Lang, Pattern) :-
	atom_codes(Lang, LC),
	atom_codes(Pattern, PC),
	langmatches_codes(PC, LC).

langmatches_codes([], []) :- !.
langmatches_codes([], [0'-|_]) :- !.
langmatches_codes([H|TP], [H|TC]) :- !,
	langmatches_codes(TP, TC).
langmatches_codes([HP|TP], [HC|TC]) :- !,
	code_type(L, to_lower(HP)),
	code_type(L, to_lower(HC)),
	langmatches_codes(TP, TC).

%%	isiri(+IRI)
%
%	True if IRI is an IRI.  We get the argument un-evaluated.

isiri(IRI) :-
	atom(IRI), !,
	\+ rdf_is_bnode(IRI).
isiri(literal(_)) :- !, fail.
isiri(Expr) :-
	eval(Expr, Value),
	Value = iri(IRI),
	\+ rdf_is_bnode(IRI).

isblank(IRI) :-
	atom(IRI), !,
	rdf_is_bnode(IRI).
isblank(literal(_)) :- !, fail.
isblank(Expr) :-
	eval(Expr, Value),
	Value = iri(IRI),
	rdf_is_bnode(IRI).

isliteral(literal(_)) :- !.
isliteral(Atom) :-
	atom(Atom), !, fail.
isliteral(Expr) :-
	eval(Expr, Value),
	Value \= iri(_).


%%	regex(+String, +Pattern, +Flags)
%
%	TBD:
%		- Avoid XPCE
%		- Complete flags

:- dynamic
	pattern_cache/3.		% Pattern, Flags, Regex

regex(String, Pattern, Flags) :-
	pattern_cache(Pattern, Flags, Regex), !,
	send(Regex, search, string(String)).
regex(String, Pattern, Flags) :-
	make_regex(Pattern, Flags, Regex),
	send(Regex, lock_object, @(on)),
	asserta(pattern_cache(Pattern, Flags, Regex)),
	send(Regex, search, string(String)).

make_regex(Pattern, i, Regex) :- !,
	new(Regex, regex(Pattern, @(off))).
make_regex(Pattern, _, Regex) :- !,
	new(Regex, regex(Pattern)).

%%	effective_boolean_value(+Expr, -Bool)
%
%	See SPARQL document, section 11.2.2: Effecitive Boolean Value

effective_boolean_value(boolean(X), boolean(True)) :- !,
	True = X.
effective_boolean_value(string(X),  boolean(True)) :- !,
	(X == '' -> True = false ; True = true).
effective_boolean_value(simple_literal(X),  boolean(True)) :- !,
	(X == '' -> True = false ; True = true).
effective_boolean_value(numeric(_, X),  boolean(True)) :- !,
	(X =:= 0 -> True = false ; True = true).
effective_boolean_value(_,  boolean(error)).

%%	sparql_eval(+Expr, -Results)
%
%	Evaluate an expression.

sparql_eval(Expr, Expr) :-
	is_rdf(Expr), !.
sparql_eval(Expr, Result) :-
	eval(Expr, Result0),
	to_rdf(Result0, Result).

to_rdf(numeric(Type, Value), literal(type(Type, Atom))) :-
	atom_number(Atom, Value).
to_rdf(boolean(Val), literal(Type, Val)) :-
	rdf_equal(xsd:boolean, Type).
to_rdf(type(T, Val), literal(type(T, Val))).
to_rdf(simple_literal(L), literal(L)).
to_rdf(iri(IRI), IRI).

%%	is_rdf(+Term)
%
%	True if Term is a valid RDF term.

is_rdf(IRI) :- atom(IRI).
is_rdf(Var) :- var(Var), !, fail.
is_rdf(literal(_)).
