/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2014, University of Amsterdam
			      VU University Amsterdam

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
	  [ sparql_true/1,		% +Expression
	    sparql_eval/2,		% +Expression, -Value
	    sparql_eval_raw/2,		% +Expression, -Value
	    sparql_simplify/2,		% :Goal, -SimpleGoal
	    sparql_subquery/3,		% +Proj, +Query, +Sols
	    sparql_update/1,		% +UpdateRequest
	    sparql_find/5,		% ?From, ?To, ?F, ?T, :Q
	    sparql_minus/2,		% :Pattern1, :Pattern2
	    sparql_group/1,		% :Query
	    sparql_group/3,		% :Query, +OuterVars, +InnerVars
	    sparql_reset_bnodes/0
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(xsdp_types)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(ordsets)).
:- use_module(library(uri)).
:- use_module(library(dcg/basics)).
:- if(exists_source(library(uuid))).
:- use_module(library(uuid)).
:- endif.

:- discontiguous
	term_expansion/2.

:- meta_predicate
	sparql_find(?, ?, ?, ?, 0),
	sparql_minus(0, 0),
	sparql_group(0),
	sparql_group(0, +, +),
	sparql_subquery(+, 0, +),
	sparql_update(:).

/** <module> SPARQL runtime support

@see	rdfql_runtime.pl merges this module with generic predicates as well
	as runtime libraries for other query languages.
@see	These routines are part of the _entailment_ modules.  See
	../entailment/README.txt
*/

:- thread_local
	bnode_store/2.

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
eval(built_in(Term), Result) :- !,
	op(Term, Result).
eval(Term, Result) :-
	sparql_op(Term), !,
	op(Term, Result).
eval(function(Term), Result) :- !,
	(   xsd_cast(Term, Type, Value0)
	->  eval(Value0, Value),
	    eval_cast(Type, Value, Result)
	;   eval_function(Term, Result)
	).
eval(Term, Term).			% Result of sub-eval

%%	eval(+Type, +Term, -Result) is semidet.
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
	eval(Term, Result),
	Result = numeric(_,_).


eval_literal(type(Type, Atom), Value) :- !,
	eval_typed_literal(Type, Atom, Value).
eval_literal(lang(Lang, Atom), lang(Lang, Atom)) :- !.
eval_literal(Atom, simple_literal(Atom)) :-
	atom(Atom), !.

eval_typed_literal(Type, Atom, numeric(Type, Value)) :-
	xsdp_numeric_uri(Type, Generic), !,
	numeric_literal_value(Generic, Atom, Value).
eval_typed_literal(Type, Atom, Value) :-
	eval_known_typed_literal(Type, Atom, Value0), !,
	Value = Value0.
eval_typed_literal(Type, Atom, type(Type, Atom)).

%%	eval_known_typed_literal(+Type, +Plain, -Typed) is semidet.
%
%	Map known datatypes to a value   that is suitable for comparison
%	using Prolog standard order of terms.  Note that the mapped time
%	representations can all be compared.

:- rdf_meta eval_known_typed_literal(r, +, t).

eval_known_typed_literal(xsd:boolean,	 Atom, boolean(Atom)).
eval_known_typed_literal(xsd:string,	 Atom, string(Atom)).
eval_known_typed_literal(xsd:gYear,	 Atom, time(xsd:gYear, Atom)).
eval_known_typed_literal(xsd:gYearMonth, Atom, time(xsd:gYearMonth, Atom)).
eval_known_typed_literal(xsd:date,	 Atom, time(xsd:date, Atom)).
eval_known_typed_literal(xsd:dateTime,	 Atom, time(xsd:dateTime, Atom)).

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
	atom(Text),
	atom_number(Text, Value),
	integer(Value).
numeric_literal_value(Type, Text, Value) :-
	rdf_equal(Type, xsd:decimal), !,
	atom(Text),
	atom_number(Text, Value).
numeric_literal_value(_, Text, Value) :-
	atom(Text),
	atom_number(Text, Value), !.
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


%	Evaluation of function arguments

eval_any(Term, Value) :-
	eval(Term, Value), !.
eval_any(_, boolean(error)).

eval_boolean(Term, Bool) :-
	eval(Term, Value),
	effective_boolean_value(Value, Bool), !.
eval_boolean(_, boolean(error)).

eval_numeric(Term, Numeric) :-
	eval(Term, Numeric),
	Numeric = numeric(_,_), !.
eval_numeric(_, boolean(error)).

%%	sparql_op(+ListOfDelcs)

term_expansion((:- sparql_op(Decls)), Clauses) :-
	maplist(decl_op, Decls, Clauses).

decl_op(Term, op_decl(Gen, Args)) :-
	functor(Term, Name, Arity),
	functor(Gen,  Name, Arity),
	Term =.. [Name|Args].


%%	expand_op(+In, -Clause) is det.
%
%	Expand SPARQL operators into a nice clause.

expand_op((op(Op,Result) :- Body),
	  [(op(Op1,Result) :- Body1), sparql_op(Op1)]) :-
	rdf_global_term(Op, Op0),
	functor(Op0, Name, Arity),
	functor(Op1, Name, Arity),
	(   op_decl(Op1, Types)
	->  true
	;   Op0 =.. [Name|Args],
	    maplist(op_arg_type, Args, Types)
	),
	Op0 =.. [Name|Args0],
	Op1 =.. [Name|Args1],
	maplist(convert_goal, Types, Args1, Args0, ConvertList),
	list_to_conj(ConvertList, Convert),
	mkconj(Convert, Body, Body1).

op_arg_type(Var,               any) :- var(Var), !.
op_arg_type(boolean(_),	       boolean) :- !.
op_arg_type(numeric(_,_),      numeric) :- !.
op_arg_type(simple_literal(_), simple_literal) :- !.
op_arg_type(_,		       any).

list_to_conj([], true).
list_to_conj([G], G) :- !.
list_to_conj([H|T], G) :-
	list_to_conj(T, G1),
	mkconj(H, G1, G).

mkconj(true, G, G) :- !.
mkconj(G, true, G) :- !.
mkconj(G1,G2,(G1,G2)).

convert_goal(no_eval, Arg, Arg, true).
convert_goal(any, Arg0, Arg1, eval_any(Arg0, Arg1)).
convert_goal(simple_literal, Arg0, Arg1, eval_any(Arg0, Arg1)).
convert_goal(boolean, Arg0, Arg1, eval_boolean(Arg0, Arg1)).
convert_goal(numeric, Arg0, Arg1, eval_numeric(Arg0, Arg1)).

term_expansion((op(Op,Result) :- Body), Clauses) :-
	expand_op((op(Op,Result) :- Body), Clauses).
term_expansion((op(Op,Result)), Clauses) :-
	expand_op((op(Op,Result) :- true), Clauses).

%%	op(+Operator, -Result) is semidet.
%
%	@param Operator	Term of the format Op(Arg...) where each Arg
%			is embedded in its type.
%	@param Result	Result-value, embedded in its type.

:- rdf_meta op(t,t).
:- discontiguous op/2, op_decl/2, sparql_op/1.

:- sparql_op([ bound(no_eval)
	     ]).

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

:- sparql_op([ iri(any, no_eval),
	       str(no_eval)
	     ]).

% SPARQL Accessors
op(str(X), simple_literal(Str)) :-
	str(X, Str).
op(lang(X), simple_literal(Lang)) :-
	lang(X, Lang).
op(datatype(X), Type) :-
	datatype(X, Type).
op(strdt(simple_literal(Lex), iri(Type)), type(Type, Lex)).
op(strlang(simple_literal(Lex), simple_literal(Lang)), lang(Lang, Lex)).
:- if(current_predicate(uuid/1)).
op(uuid, iri(URNUUID)) :-
	uuid(UUID),
	atom_concat('urn:uuid:', UUID, URNUUID).
op(struuid, simple_literal(UUID)) :-
	uuid(UUID).
:- endif.
op(bnode, iri(Id)) :-
	rdf_bnode(Id).
op(bnode(simple_literal(Id)), iri(BNode)) :-
	id_to_bnode(Id, BNode).
op(iri(Spec, Base), iri(URI)) :-
	iri(Spec, Base, URI).

% SPARQL Binary operators
% Logical connectives, defined in section 11.4
op(and(boolean(A), boolean(B)), boolean(Result)) :-
	sparql_and(A, B, Result).
op(or(boolean(A), boolean(B)), boolean(Result)) :-
	sparql_or(A, B, Result).

:- sparql_op([ coalesce(no_eval)
	     ]).

% SPARQL functional forms
op(if(Test, V1, V2), Result) :-
	typed_eval(boolean, Test, TestResult),
	(   TestResult == boolean(true)
	->  eval(V1, Result)
	;   TestResult == boolean(false)
	->  eval(V2, Result)
	).
op(coalesce(List), Result) :-
	member(Expr, List),
	ground(Expr),
	eval(Expr, Result),
	\+ invalid(Result), !.

invalid('$null$').
invalid(boolean(error)).

% XPath Tests
op(X = Y, boolean(Result)) :-
	(   equal(X, Y)
	->  Result = true
	;   Result = false
	).
op(X \= Y, boolean(Result)) :-
	(   equal(X, Y)
	->  Result = false
	;   Result = true
	).

equal(X, X) :- !.
equal(numeric(_, X), numeric(_, Y)) :- X =:= Y.
equal(boolean(A), boolean(B)) :-
	eq_bool(A, B, true).

op(X < Y, boolean(Result)) :-
	(   lt(X,Y)
	->  Result = true
	;   functor(X, Name, Arity),
	    functor(Y, Name, Arity)
	->  Result = false
	).
op(X > Y, boolean(Result)) :-
	(   gt(X,Y)
	->  Result = true
	;   functor(X, Name, Arity),
	    functor(Y, Name, Arity)
	->  Result = false
	).
op(X =< Y, boolean(Result)) :-
	(   leq(X,Y)
	->  Result = true
	;   functor(X, Name, Arity),
	    functor(Y, Name, Arity)
	->  Result = false
	).
op(X >= Y, boolean(Result)) :-
	(   geq(X,Y)
	->  Result = true
	;   functor(X, Name, Arity),
	    functor(Y, Name, Arity)
	->  Result = false
	).

lt(numeric(_, X), numeric(_, Y)) :- X < Y.
lt(simple_literal(X), simple_literal(Y)) :- X @< Y.
lt(string(X), string(Y)) :- X @< Y.
lt(time(T, X), time(T, Y)) :- X @< Y.
lt(type(T, X), type(T, Y)) :- X @< Y.

gt(numeric(_, X), numeric(_, Y)) :- X > Y.
gt(simple_literal(X), simple_literal(Y)) :- X @> Y.
gt(string(X), string(Y)) :- X @> Y.
gt(time(T, X), time(T, Y)) :- X @> Y.
gt(type(T, X), type(T, Y)) :- X @> Y.

leq(numeric(_, X), numeric(_, Y)) :- X =< Y.
leq(simple_literal(X), simple_literal(Y)) :- X @=< Y.
leq(string(X), string(Y)) :- X @=< Y.
leq(time(T, X), time(T, Y)) :- X @=< Y.
leq(type(T, X), type(T, Y)) :- X @=< Y.

geq(numeric(_, X), numeric(_, Y)) :- X >= Y.
geq(simple_literal(X), simple_literal(Y)) :- X @>= Y.
geq(string(X), string(Y)) :- X @>= Y.
geq(time(T, X), time(T, Y)) :- X @>= Y.
geq(type(T, X), type(T, Y)) :- X @>= Y.

% arithmetic
op(numeric(TX, X) * numeric(TY, Y), numeric(Type, Result)) :-
	Result is X * Y,
	combine_types(TX, TY, Type).
op(numeric(TX, X) / numeric(TY, Y), numeric(Type, Result)) :-
	Y =\= 0,
	Result is X / Y,
	combine_types_div(TX, TY, Type).
op(numeric(TX, X) + numeric(TY, Y), numeric(Type, Result)) :-
	Result is X + Y,
	combine_types(TX, TY, Type).
op(numeric(TX, X) - numeric(TY, Y), numeric(Type, Result)) :-
	Result is X - Y,
	combine_types(TX, TY, Type).
% arithmetic to support aggregates
op(min(numeric(TX, X), numeric(TY, Y)), numeric(Type, Result)) :-
	(   X < Y
	->  Type = TX, Result = X
	;   X > Y
	->  Type = TY, Result = Y
	;   combine_types(TX, TY, Type),
	    (	Type == TX
	    ->	Result = X
	    ;	Result = Y
	    )
	).
op(max(numeric(TX, X), numeric(TY, Y)), numeric(Type, Result)) :-
	(   X > Y
	->  Type = TX, Result = X
	;   X < Y
	->  Type = TY, Result = Y
	;   combine_types(TX, TY, Type),
	    (	Type == TX
	    ->	Result = X
	    ;	Result = Y
	    )
	).

% SPARQL Tests, defined in section 11.4

op(in(Value, List), boolean(Result)) :-
	sparql_in(Value, List, Result).
op(not_in(Value, List), boolean(Result)) :-
	sparql_in(Value, List, R0),
	not(R0, Result).

sparql_in(Value, List, Result) :-
	(   memberchk(Value, List)
	->  Result = true
	;   member(E, List),
	    eval(E, EV),
	    rdf_equal(Value, EV)
	->  Result = true
	;   Result = false
	).


% SPARQL builtin string functions (1.1)

:- sparql_op([ strlen(any),
	       substr(any, numeric),
	       substr(any, numeric, numeric),
	       ucase(any),
	       lcase(any),
	       strstarts(any, any),
	       strends(any, any),
	       contains(any, any),
	       strbefore(any, any),
	       strafter(any, any),
	       encode_for_uri(any),
	       concat(no_eval)
	     ]).

op(strlen(A), numeric(xsd:integer, Len)) :-
	string_op(A, Len, strlen).
op(substr(A, numeric(xsd:integer, Start)), R) :-
	string_int_op_string(A, Start, R, substr).
op(substr(A, numeric(xsd:integer, Start), numeric(xsd:integer, Len)), R) :-
	string_int_int_op_string(A, Start, Len, R, substr).
op(ucase(A), U) :-
	string_op_string(A, U, ucase).
op(lcase(A), U) :-
	string_op_string(A, U, lcase).
op(strstarts(String, Starts), boolean(True)) :-
	argument_compatible(String, Starts, True, strstarts).
op(strends(String, Starts), boolean(True)) :-
	argument_compatible(String, Starts, True, strends).
op(contains(String, Starts), boolean(True)) :-
	argument_compatible(String, Starts, True, contains).
op(strbefore(A1, A2), R) :-
	string_string_op(A1, A2, R, strbefore).
op(strafter(A1, A2), R) :-
	string_string_op(A1, A2, R, strafter).
op(encode_for_uri(S), simple_literal(URI)) :-
	str_value(S, Text),
	uri_encoded(path, Text, IRI),
	uri_iri(URI, IRI).
op(concat(List), R) :-
	maplist(eval, List, Evaluated),
	maplist(str_text, Evaluated, StrList),
	atomic_list_concat(StrList, Lex),
	(   maplist(is_string, Evaluated)
	->  R = string(Lex)
	;   maplist(is_lang(L), Evaluated)
	->  R = lang(L, Lex)
	;   R = simple_literal(Lex)
	).
op(langmatches(simple_literal(Lang),
	       simple_literal(Pat)),
   boolean(Result)) :-
	(lang_matches(Lang, Pat) -> Result = true ; Result = false).
op(regex(A, simple_literal(Pat)), boolean(Result)) :-
	string_op(A, Result, regex(Pat, '')).
op(regex(A, simple_literal(Pat), simple_literal(Flags)), boolean(Result)) :-
	string_op(A, Result, regex(Pat, Flags)).
op(compiled_regex(Regex, A), boolean(Result)) :-
	string_op(A, Result, compiled_regex(Regex)).
op(replace(simple_literal(Input),
	   simple_literal(Pattern),
	   simple_literal(Replace),
	   simple_literal(Flags)),
   simple_literal(Result)) :-
	regex_replace(Input, Pattern, Replace, Flags, Result).
op(replace(string(Input),
	   simple_literal(Pattern),
	   simple_literal(Replace),
	   simple_literal(Flags)),
   string(Result)) :-
	regex_replace(Input, Pattern, Replace, Flags, Result).
op(replace(lang(Lang, Input),
	   simple_literal(Pattern),
	   simple_literal(Replace),
	   simple_literal(Flags)),
   lang(Lang, Result)) :-
	regex_replace(Input, Pattern, Replace, Flags, Result).

% SPARQL builtin numeric functions (1.1, 17.4.4)

:- sparql_op([ isnumeric(any)
	     ]).

op(isnumeric(A), boolean(True)) :-
	( A = numeric(_,_) ->  True = true ; True = false ).
op(abs(numeric(T, A1)), numeric(T, R)) :-
	R is abs(A1).
op(round(numeric(T, A1)), numeric(T, R)) :-
	R is round(A1).
op(ceil(numeric(T, A1)), numeric(T, R)) :-
	R is ceil(A1).
op(floor(numeric(T, A1)), numeric(T, R)) :-
	R is floor(A1).
op(rand, numeric(xsd:double, R)) :-
	R is random_float.

% SPARQL builtin date and time functions (1.1, 17.4.5)

op(now, time(xsd:dateTime, Date)) :-
	get_time(Now),
	format_time(atom(Date), '%FT%T.%3f%:z', Now).
op(year(time(Type, DateTime)), numeric(xsd:integer, Year)) :-
	time_part(year, Type, DateTime, Year).
op(month(time(Type, DateTime)), numeric(xsd:integer, Month)) :-
	time_part(month, Type, DateTime, Month).
op(day(time(Type, DateTime)), numeric(xsd:integer, Day)) :-
	time_part(day, Type, DateTime, Day).
op(hours(time(Type, DateTime)), numeric(xsd:integer, Hours)) :-
	time_part(hours, Type, DateTime, Hours).
op(minutes(time(Type, DateTime)), numeric(xsd:integer, Minutes)) :-
	time_part(minutes, Type, DateTime, Minutes).
op(seconds(time(Type, DateTime)), numeric(xsd:decimal, Seconds)) :-
	time_part(seconds, Type, DateTime, Seconds).
op(timezone(time(Type, DateTime)), type(xsd:dayTimeDuration, Timezone)) :-
	time_part(tzs, Type, DateTime, TZs),
	phrase(tz_offset(TZOffset), TZs),
	xsd_duration_seconds(Timezone, TZOffset).
op(tz(time(Type, DateTime)), simple_literal(TZ)) :-
	time_part(tz, Type, DateTime, TZ).

% SPARQL builtin hash functions (1.1, 17.4.6)

:- sparql_op([ md5(any),
	       sha1(any),
	       sha256(any),
	       sha384(any),
	       sha512(any)
	     ]).

op(md5(String), simple_literal(Hash)) :-
	string_hash(String, Hash, md5).
op(sha1(String), simple_literal(Hash)) :-
	string_hash(String, Hash, sha1).
op(sha256(String), simple_literal(Hash)) :-
	string_hash(String, Hash, sha256).
op(sha384(String), simple_literal(Hash)) :-
	string_hash(String, Hash, sha384).
op(sha512(String), simple_literal(Hash)) :-
	string_hash(String, Hash, sha512).


		 /*******************************
		 *   HASH SUPPORT FUNCTIONS	*
		 *******************************/

string_hash(simple_literal(S), Hash, Algorithm) :-
	atom_hash(Algorithm, S, Hash).
string_hash(string(S), Hash, Algorithm) :-
	atom_hash(Algorithm, S, Hash).

atom_hash(md5, S, Hash) :- !,
	rdf_atom_md5(S, 1, Hash).
atom_hash(SHA, S, Hash) :-
	sha_hash(S, HashCodes,
		 [ algorithm(SHA),
		   encoding(utf8)
		 ]),
	hash_atom(HashCodes, Hash).


		 /*******************************
		 *    TIME SUPPORT FUNCTIONS	*
		 *******************************/

%%	time_part(+Part, +Type, +String, -Value) is semidet.

:- if(current_predicate(sub_string/5)).
time_part(year, _Type, String, Value) :- !,
	sub_string(String, 0, 4, _, Digits),
	number_string(Value, Digits).
time_part(month, _Type, String, Value) :- !,
	sub_string(String, 5, 2, _, Digits),
	number_string(Value, Digits).
time_part(day, _Type, String, Value) :- !,
	sub_string(String, 8, 2, _, Digits),
	number_string(Value, Digits).
:- endif.
time_part(Part, _Type, DateTime, Value) :-
	atom_codes(DateTime, Codes),
	phrase(time_dcg(Part, Value), Codes, _).

time_dcg(year,  Year)  --> digits4(Year).
time_dcg(month, Month) --> time_dcg(year, _),    "-", digits2(Month).
time_dcg(day,   Day)   --> time_dcg(month, _),   "-", digits2(Day).
time_dcg(hours, Hours) --> time_dcg(day, _),     "T", digits2(Hours).
time_dcg(minutes, Min) --> time_dcg(hours, _),   ":", digits2(Min).
time_dcg(seconds, Sec) --> time_dcg(minutes, _), ":", number(Sec).
time_dcg(tzs,     TZs) --> time_dcg(seconds, _),      string_without("", TZs).
time_dcg(tz,      TZ)  --> time_dcg(tzs, TZs), { atom_codes(TZ, TZs) }.

tz_offset(TZOffset) -->
	"Z", !, { TZOffset = 0 }.
tz_offset(TZOffset) -->
	"+", digits2(Hours), ":", digits2(Minutes),
	{ TZOffset is Hours*3600+Minutes*60 }.
tz_offset(TZOffset) -->
	"-", digits2(Hours), ":", digits2(Minutes),
	{ TZOffset is -(Hours*3600+Minutes*60) }.

%%	seconds_xsd_duration(+Seconds, -XSDDuration)
%
%	@see http://docs.oracle.com/cd/E13214_01/wli/docs92/xref/xqdurfunc.html#wp1183764
%	@tbd	Implement other direction and move this to XSD or datetime
%		library.

xsd_duration_seconds(XSDDuration, Secs) :-
	var(XSDDuration), !,
	must_be(number, Secs),
	phrase(xsd_duration(Secs), Codes),
	atom_codes(XSDDuration, Codes).

xsd_duration(Secs) -->
	{ Secs < 0, !, PosSecs is -Secs },
	"-",
	xsd_duration(PosSecs).
xsd_duration(Secs) -->
	{ Secs =:= 0 }, !,
	"PT0S".
xsd_duration(Secs) -->
	"P",
	xsd_duration_days(Secs, Rem),
	xsd_duration_time(Rem).

xsd_duration_days(Secs, Rem) -->
	{ Days is Secs // (24*3600),
	  Days > 0, !,
	  Rem is Secs - Days*24*3600
	},
	integer(Days),
	"D".
xsd_duration_days(Secs, Secs) --> "".

xsd_duration_time(Secs) -->
	{ Secs =:= 0 }, !.
xsd_duration_time(Secs) -->
	"T",
	xsd_duration_hours(Secs, S1),
	xsd_duration_minutes(S1, S2),
	xsd_duration_seconds(S2).

xsd_duration_hours(Secs, Rem) -->
	{ Hours is Secs // 3600,
	  Hours > 0, !,
	  Rem is Secs - Hours*3600
	},
	integer(Hours),
	"H".
xsd_duration_hours(Secs, Secs) --> "".

xsd_duration_minutes(Secs, Rem) -->
	{ Min is Secs // 60,
	  Min > 0, !,
	  Rem is Secs - Min*60
	},
	integer(Min),
	"M".
xsd_duration_minutes(Secs, Secs) --> "".

xsd_duration_seconds(Secs) -->
	{ Secs =:= 0 }, !.
xsd_duration_seconds(Secs) -->
	number(Secs),
	"S".


digits4(Value) -->
	digit(D1),digit(D2),digit(D3),digit(D4),
	{ number_codes(Value, [D1,D2,D3,D4]) }.
digits2(Value) -->
	digit(D1),digit(D2),
	{ number_codes(Value, [D1,D2]) }.


		 /*******************************
		 *  STRING SUPPORT PRIMITIVES	*
		 *******************************/

is_string(string(_)).
is_lang(L, lang(L,_)).

%%	string_op1(+A1, -R, +Op)

string_op(simple_literal(A), R, Op) :-
	atom_op(Op, A, R).
string_op(lang(_, A), R, Op) :-
	atom_op(Op, A, R).
string_op(string(A), R, Op) :-
	atom_op(Op, A, R).

%%	string_op_string(+A, -R)

string_op_string(simple_literal(A), simple_literal(R), Op) :-
	atom_op(Op, A, R).
string_op_string(lang(L,A), lang(L,R), Op) :-
	atom_op(Op, A, R).
string_op_string(string(A), string(R), Op) :-
	atom_op(Op, A, R).

%%	string_int_op_string(+S0, +I, -S)

string_int_op_string(simple_literal(S0), I, simple_literal(S), Op) :-
	atom_op(Op, S0, I, S).
string_int_op_string(lang(L, S0), I, lang(L, S), Op) :-
	atom_op(Op, S0, I, S).
string_int_op_string(string(S0), I, string(S), Op) :-
	atom_op(Op, S0, I, S).

%%	string_int_int_op_string(+S0, +I, -S)

string_int_int_op_string(simple_literal(S0), I1, I2, simple_literal(S), Op) :-
	atom_op(Op, S0, I1, I2, S).
string_int_int_op_string(lang(L, S0), I1, I2, lang(L, S), Op) :-
	atom_op(Op, S0, I1, I2, S).
string_int_int_op_string(string(S0), I1, I2, string(S), Op) :-
	atom_op(Op, S0, I1, I2, S).

%%	string_op2(+A1, +A2, -R, +Op)
%
%	Define operations on strings.

string_string_op(simple_literal(A1), simple_literal(A2), Result, Op) :-
	(   atom_op(Op, A1, A2, R)
	->  Result = simple_literal(R)
	;   Result = simple_literal('')
	).
string_string_op(simple_literal(A1), string(A2), Result, Op) :-
	(   atom_op(Op, A1, A2, R)
	->  Result = simple_literal(R)
	;   Result = simple_literal('')
	).
string_string_op(string(A1), simple_literal(A2), Result, Op) :-
	(   atom_op(Op, A1, A2, R)
	->  Result = string(R)
	;   Result = simple_literal('')
	).
string_string_op(string(A1), string(A2), Result, Op) :-
	(   atom_op(Op, A1, A2, R)
	->  Result = string(R)
	;   Result = simple_literal('')
	).
string_string_op(lang(L, A1), lang(L, A2), Result, Op) :-
	(   atom_op(Op, A1, A2, R)
	->  Result = lang(L, R)
	;   Result = simple_literal('')
	).
string_string_op(lang(L, A1), string(A2), Result, Op) :-
	(   atom_op(Op, A1, A2, R)
	->  Result = lang(L, R)
	;   Result = simple_literal('')
	).
string_string_op(lang(L, A1), simple_literal(A2), Result, Op) :-
	(   atom_op(Op, A1, A2, R)
	->  Result = lang(L, R)
	;   Result = simple_literal('')
	).

%%	iri(+Spec, +Base, -IRI)

iri(simple_literal(URI0), Base, URI) :- !,
	uri_normalized(URI0, Base, URI).
iri(string(URI0), Base, URI) :-
	uri_normalized(URI0, Base, URI).
iri(iri(URI), _, URI).

%%	argument_compatible(+A1, +A2, -Bool, +Op)

argument_compatible(simple_literal(A1), simple_literal(A2), Bool, Op) :- !,
	arg_compatible(Op, A1, A2, Bool).
argument_compatible(simple_literal(A1), string(A2), Bool, Op) :- !,
	arg_compatible(Op, A1, A2, Bool).
argument_compatible(string(A1), simple_literal(A2), Bool, Op) :- !,
	arg_compatible(Op, A1, A2, Bool).
argument_compatible(string(A1), string(A2), Bool, Op) :- !,
	arg_compatible(Op, A1, A2, Bool).
argument_compatible(lang(L,A1), lang(L,A2), Bool, Op) :- !,
	arg_compatible(Op, A1, A2, Bool).
argument_compatible(lang(_,A1), simple_literal(A2), Bool, Op) :- !,
	arg_compatible(Op, A1, A2, Bool).
argument_compatible(lang(_,A1), string(A2), Bool, Op) :- !,
	arg_compatible(Op, A1, A2, Bool).
argument_compatible(_, _, boolean(error), _).

arg_compatible(Op, A1, A2, Bool) :-
	(   arg_compatible(Op, A1, A2)
	->  Bool = true
	;   Bool = false
	).

arg_compatible(strstarts, A1, A2) :- sub_atom(A1, 0, _, _, A2).
arg_compatible(strends,   A1, A2) :- sub_atom(A1, _, _, 0, A2).
arg_compatible(contains,  A1, A2) :- sub_atom(A1, _, _, _, A2), !.


%%	atom_op(+Op, +Atom, -Result).

atom_op(strlen, A, Len) :-
	atom_length(A, Len).
atom_op(ucase, A, U) :-
	upcase_atom(A, U).
atom_op(lcase, A, U) :-
	downcase_atom(A, U).
atom_op(compiled_regex(Regex), Data, Matches) :-
	(   compiled_regex(Regex, Data)
	->  Matches = true
	;   Matches = false
	).
atom_op(regex(Pat, Flags), Data, Matches) :-
	(   regex(Data, Pat, Flags)
	->  Matches = true
	;   Matches = false
	).

%%	atom_op(+Op, +Atom, +Arg, -Result).

atom_op(substr, Atom, Start, Sub) :-
	S is Start - 1,
	(   sub_atom(Atom, S, _, 0, Sub0)
	->  Sub = Sub0
	;   Sub = ''			% is this ok?
	).
atom_op(strbefore, Atom, Search, Before) :-
	(   Search == ''
	->  Before = ''
	;   sub_atom(Atom, BL, _, _, Search)
	->  sub_atom(Atom, 0, BL, _, Before)
	).
atom_op(strafter, Atom, Search, After) :-
	(   sub_atom(Atom, _, _, AL, Search)
	->  sub_atom(Atom, _, AL, 0, After)
	).

%%	atom_op(+Op, +Atom, +A1, +A2, -Result).

atom_op(substr, Atom, Start, Len, Sub) :-
	S is Start - 1,
	(   sub_atom(Atom, S, Len, _, Sub0)
	->  Sub = Sub0
	;   sub_atom(Atom, S, _, 0, Sub0)
	->  Sub = Sub0
	;   Sub = ''			% is this ok?
	).


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

:- rdf_meta rdf_equal(t,t,-).

rdf_equal(X, X, boolean(true)) :- !.
rdf_equal(boolean(A), boolean(B),  boolean(Eq)) :- !,
	eq_bool(A, B, Eq).
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


		 /*******************************
		 *	       CASTS		*
		 *******************************/

%%	xsd_cast(+Term, -Type, -Arg)
%
%	Deals with xsd:dateTime(?a), casting ?a to   the XML Schema type
%	dateTime. Supported types are the numeric types, xsd:boolean and
%	xsd:dateTime.

term_expansion(xsd_cast(term,type,arg), Clauses) :-
	findall(Clause, xsd_cast_clause(Clause), Clauses).

xsd_cast_clause(xsd_cast(Term, Type, Arg)) :-
	(   xsdp_numeric_uri(Type, _)
	;   rdf_equal(xsd:dateTime, Type)
	;   rdf_equal(xsd:boolean, Type)
	),
	Term =.. [Type,Arg].

xsd_cast(term,type,arg).

%%	eval_cast(+Type, +Value, -Result)
%
%	Cast Value to Type, resulting  in   a  typed  literal. Currently
%	casts plain literals to the requested type and numeric values to
%	other numeric values.

eval_cast(Type, simple_literal(Value), Result) :-
	atom(Value), !,
	eval_typed_literal(Type, Value, Result).
eval_cast(Type, numeric(_, Value0), numeric(Type, Value)) :-
	xsdp_numeric_uri(Type, Generic),
	(   rdf_equal(Generic, xsd:integer)
	->  Value is integer(Value0)
	;   (   rdf_equal(Generic, xsd:float)
	    ;   rdf_equal(Generic, xsd:double)
	    )
	->  Value is float(Value0)
	;   Value = Value0
	).


%%	eval_function(+Term, -Result)
%
%	Eval user-defined function.  User-defined functions are of the
%	form sparql:function(Term, Result).

:- multifile
	sparql:function/2,
	sparql:current_function/1.

eval_function(Term0, Result) :-
	Term0 =.. [F|Args0],
	eval_args(Args0, Args),
	Term =.. [F|Args],
	sparql:function(Term, Result0), !,
	eval(Result0, Result).
eval_function(Term, boolean(error)) :-
	sparql:current_function(Term), !.
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
not(error, error).

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

str_value(simple_literal(X), X).
str_value(lang(_, X), X).
str_value(boolean(X), X).
str_value(string(X), X).
str_value(iri(IRI), IRI).

str_literal(type(_, Str), Str) :- !.
str_literal(lang(_, Str), Str) :- !.
str_literal(Str, Str).

str_text(simple_literal(X), X).
str_text(lang(_, X), X).
str_text(string(X), X).


%%	lang(+RDFTerm, -Lang)
%
%	Extract language specification from an RDFTerm

lang(lang(Lang,_), Lang) :- !.
lang(string(_), '').
lang(simple_literal(_), '').
lang(type(_,_), '').
lang(numeric(_,_), '').

%%	datatype(+RDFTerm, -IRI)
%
%	Extract type specification from an RDFTerm

:- rdf_meta
	datatype(t,t).

datatype(0, _) :- !, fail.
datatype(literal(type(Type, _)), iri(Type)) :- !.
datatype(numeric(Type, _), iri(Type)) :- !.
datatype(boolean(_), iri(xsd:boolean)) :- !.
datatype(time(Type, _), iri(Type)) :- !.
datatype(string(_), iri(xsd_string)) :- !.
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
	with_mutex(sparql_regex,
		   ( regex_obj(Pattern, Flags, Regex),
		     send(Regex, search, string(String)))).

regex_obj(Pattern, Flags, Regex) :-
	pattern_cache(Pattern, Flags, Regex), !.
regex_obj(Pattern, Flags, Regex) :-
	make_regex(Pattern, Flags, Regex),
	asserta(pattern_cache(Pattern, Flags, Regex)).

make_regex(Pattern, i, Regex) :- !,
	new(Regex, regex(Pattern, @(off))).
make_regex(Pattern, _, Regex) :- !,
	new(Regex, regex(Pattern)).

%%	compiled_regex(+Compiled, +Text) is semidet.
%
%	Test using a regex that has   been  prepared. Compiled takes the
%	following forms:
%
%	  - XPCE object

compiled_regex(@(Regex), String) :-
	send(@(Regex), search, string(String)).

%%	regex_replace(+Input, +Pattern, +Replace, +Flags, -Result)

regex_replace(Input, Pattern, Replace0, Flags, Result) :-
	dollar_replace(Replace0, Replace),
	with_mutex(sparql_regex,
		   locked_replace(Input, Pattern, Replace, Flags, Result)).

dollar_replace(Replace0, Replace) :-
	sub_atom(Replace0, _, _, _, $), !,
	regex_replace(Replace0, '\\$([0-9])', '\\\\1', '', Replace).
dollar_replace(Replace, Replace).


locked_replace(Input, Pattern, Replace, Flags, Result) :-
	regex_obj(Pattern, Flags, Regex),
	new(S, string('%s', Input)),
	send(Regex, for_all, S,
	     message(@arg1, replace, @arg2, Replace)),
	get(S, value, Result).



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
%	Evaluate a SPARQL expression.

sparql_eval(Expr, Expr) :-
	is_rdf(Expr), !.
sparql_eval(Expr, Result) :-
	eval(Expr, Result0), !,
	to_rdf(Result0, Result).
sparql_eval(Expr, '$null$') :-
	debug(sparql(eval), '~p --> NULL', [Expr]).

%%	sparql_eval_raw(+Expr, -Result)
%
%	Same as sparql_eval/2, but return the raw result.

sparql_eval_raw(Expr, Result) :-
	(   eval(Expr, Result0)
	->  Result = Result0
	;   Result = '$null$',
	    debug(sparql(eval), '~p --> NULL', [Expr])
	).

:- rdf_meta
	to_rdf(+,t).

to_rdf(numeric(Type, Value), literal(type(Type, Atom))) :- !,
	atom_number(Atom, Value).
to_rdf(boolean(Val), literal(type(xsd:boolean, Val))) :- !.
to_rdf(type(T, Val), literal(type(T, Val))) :- !.
to_rdf(lang(L, Val), literal(lang(L, Val))) :- !.
to_rdf(simple_literal(L), literal(L)) :- !.
to_rdf(string(L), literal(type(xsd:string, L))) :- !.
to_rdf(time(Type, D), literal(type(Type, D))) :- !.
to_rdf(iri(IRI), IRI) :- !.
to_rdf(X, X) :- is_rdf(X).

%%	is_rdf(+Term)
%
%	True if Term is a valid RDF term.

is_rdf(IRI) :- atom(IRI).
is_rdf(Var) :- var(Var), !, fail.
is_rdf(literal(_)).


		 /*******************************
		 *     PROPERTY PATH SUPPORT	*
		 *******************************/

%%	sparql_find(?From, ?To, ?F, ?T, :Q) is nondet.
%
%	Implement *(PropertyPath). We should probably collect translated
%	queries in a dynamic predicate to   avoid the copy_term. Also, Q
%	will quite often  be  simple.  In  that   case  we  can  map  to
%	rdf_reachable/3,  although  one  of   the    problems   is  that
%	rdf_reachable/3 uses rdf_has/3, and does not deal with graphs.
%
%	We should be a bit  smarter   here  and  choose between forward,
%	backward, two-sided breath-first, etc.  based   on  which  start
%	point is given.
%
%	@tbd	Maybe a thing for using tor?  Planning most likely more
%		important than the iteration speed.

sparql_find(From, To, F, T, Q) :-
	empty_assoc(Visited),
	(   nonvar(From)
	->  sparql_find_f(From, To, F, T, Q, Visited)
	;   nonvar(To)
	->  sparql_find_b(From, To, F, T, Q, Visited)
	;   query_graph(Q, Graph)
	->  rdf_current_node(Graph, From),
	    sparql_find_f(From, To, F, T, Q, Visited)
	;   rdf_current_node(From),
	    sparql_find_f(From, To, F, T, Q, Visited)
	).

sparql_find_f(Place, Place, _, _, _, _).
sparql_find_f(From, To, F, T, Q, Visited) :-
	copy_term(t(F,T,Q), t(From, Tmp, Q2)),
	call(Q2),
	\+ get_assoc(Tmp, Visited, _),
	put_assoc(Tmp, Visited, true, V2),
	sparql_find_f(Tmp, To, F, T, Q, V2).


sparql_find_b(Place, Place, _, _, _, _).
sparql_find_b(From, To, F, T, Q, Visited) :-
	copy_term(t(F,T,Q), t(Tmp, To, Q2)),
	call(Q2),
	\+ get_assoc(Tmp, Visited, _),
	put_assoc(Tmp, Visited, true, V2),
	sparql_find_b(From, Tmp, F, T, Q, V2).


%%	query_graph(+Query, -Graph) is semidet.
%
%	True when Query is associated with graph.  Note that property
%	paths are always executed in a single graph.

query_graph(V, _) :-
	var(V), !, fail.
query_graph(_:Q, G) :-
	query_graph(Q, G).
query_graph((A,B), G) :-
	(   query_graph(A, G)
	;   query_graph(B, G)
	).
query_graph((A;B), G) :-
	(   query_graph(A, G)
	;   query_graph(B, G)
	).
query_graph((A->B), G) :-
	(   query_graph(A, G)
	;   query_graph(B, G)
	).
query_graph((A*->B), G) :-
	(   query_graph(A, G)
	;   query_graph(B, G)
	).
query_graph(rdf(_,_,_,G:_), G).


%%	rdf_current_node(?Graph, -Resource)
%
%	True when Resource is a resource in Graph.  This means it is
%	either a subject or an object of a triple in Graph.

rdf_current_node(Graph, R) :-
	rdf_graph(Graph),
	setof(R,
	      (	rdf(S,_,O,Graph),
	        (   R = S
		;   atom(O),
		    R = O
		)
	      ),
	      Rs),
	member(R, Rs).


%%	rdf_current_node(-Resource)
%
%	Generates all known resources on backtracing.   This is there to
%	support {?s :p* ?o}. A highly dubious query.

rdf_current_node(From) :-
	rdf_subject(From).
rdf_current_node(From) :-
	findall(R, (rdf(_,_,R), \+ (atom(R), rdf_subject(R))), Rs),
	sort(Rs, Set),
	member(From, Set).


%%	sparql_minus(:QLeft, :QRight)
%
%	Realise SPARQL =MINUS=.  This is defined to
%
%	    - Take the variables of QLeft
%	    - Determine the result-set for these variables for
%	      both QLeft and QRight
%	    - Substract those from QLeft that are in QRight
%
%	@tbd:	Both the result set and the minus set are in standard
%		order of terms, so we can do ordered subtraction.

sparql_minus(QLeft, QRight) :-
	term_variables(QLeft,  VarsLeft0),  sort(VarsLeft0,  VarsLeft),
	term_variables(QRight, VarsRight0), sort(VarsRight0, VarsRight),
	ord_intersection(VarsLeft, VarsRight, VarsCommon),
	(   VarsCommon == []
	->  QLeft
	;   ord_subtract(VarsLeft, VarsCommon, ExtraLeft),
	    VLeft =.. [v|ExtraLeft],
	    VCommon =.. [v|VarsCommon],
	    findall(VCommon-VLeft, (QLeft,cond_bind_null(VarsLeft)), AllSols),
	    AllSols \== [],
	    sort(AllSols, AllSorted),
	    findall(VCommon, (QRight,cond_bind_null(VarsCommon)), MinusSols),
	    sort(MinusSols, MinusSorted),
	    member(VCommon-VLeft, AllSorted),
	    \+ memberchk(VCommon, MinusSorted)
	).

cond_bind_null([]).
cond_bind_null([H|T]) :-
	(   var(H)
	->  H = '$null$'
	;   true
	),
	cond_bind_null(T).


		 /*******************************
		 *	   SPARQL GROUP		*
		 *******************************/

%%	sparql_group(:Goal)
%
%	Same as call.  Intended to keep groups together to avoid invalid
%	optimizations.

sparql_group(Goal) :-
	call(Goal).

%%	sparql_group(:Goal, +OuterVars, +InnerVars)
%
%	Execute a group that contains non-steadfast variables, which
%	asks for delayed unification of the output arguments.

sparql_group(Goal, OuterVars, InnerVars) :-
	call(Goal),
	OuterVars = InnerVars.


		 /*******************************
		 *	       BNODES		*
		 *******************************/

%%	sparql_reset_bnodes
%
%	Reset the database for the BNODE(str) function

sparql_reset_bnodes :-
	retractall(bnode_store(_,_)).


		 /*******************************
		 *	      SIMPLIFY		*
		 *******************************/

%%	sparql_simplify(:Goal, -Simple) is det.
%
%	Simplify goals to the SPARQL runtime functions before they are
%	handed to the general optimizer and runtime evaluation.

sparql_simplify(sparql_true(E), G) :-
	simplify_true(E, G), !.
sparql_simplify(sparql_eval(E, V), G) :-
	simplify_eval(E, V, G), !.
sparql_simplify(Goal, Goal).


%%	simplify_true(+Expr, -Goal) is semidet.
%
%	Simplify a boolean expression  resulting   from  a SPARQL FILTER
%	statement. Ideally, this should be   a simple partial evaluation
%	of sparql_true/1.

simplify_true(Var, Var) :-		% E.g., FILTER(?a)
	var(Var), !,
	fail.
simplify_true(or(A0,B0), (A;B)) :- !,
	simplify_true(A0, A),
	simplify_true(B0, B).
simplify_true(and(A0,B0), (A,B)) :- !,
	simplify_true(A0, A),
	simplify_true(B0, B).
simplify_true(A0=B0, A=B) :- !,
	peval(A0, A, IsResource),
	peval(B0, B, IsResource),
	IsResource == true.		% at least one is a resource
simplify_true(A0\=B0, A\=B) :- !,
	peval(A0, A, IsResource),
	peval(B0, B, IsResource),
	IsResource == true.		% at least one is a resource
simplify_true(Expr, sparql_true(PExpr)) :-
	simplify_expression(Expr, PExpr).

simplify_expression(Var, Var) :-
	var(Var), !.
simplify_expression(Term0, Term) :-
	ground(Term0), !,
	eval(Term0, Term).
simplify_expression(Term0, Term) :-
	list_arg(Term0), !,
	Term0 =.. [Name,Args0],
	maplist(simplify_expression, Args0, Args),
	Term =.. [Name,Args].
simplify_expression(Term0, Term) :-
	compound(Term0), !,
	Term0 =.. [Name|Args0],
	maplist(simplify_expression, Args0, Args),
	Term1 =.. [Name|Args],
	simplify_test(Term1, Term).
simplify_expression(Term, Term).

list_arg(concat(_)).
list_arg(coalesce(_)).

%%	simplify_test(+Expr0, -Expr) is det.
%
%	Perform analysis on specific tests.   Currently  optimizes regex
%	tests.

simplify_test(regex(String, simple_literal(Pattern), simple_literal(Flags)),
	      compiled_regex(Regex, String)) :-
	atom(Pattern), atom(Flags), !,
	regex_obj(Pattern, Flags, Regex).
simplify_test(Expr, Expr).

%%	simplify_eval(+Expr, +Value, -Goal) is semidet.

simplify_eval(Expr, Var, Goal) :-
	simplify_expression(Expr, Expr1),
	Goal = sparql_eval(Expr1, Var).

peval(Var, Var, IsResource) :-
	var(Var), !,
	(   get_attr(Var, annotations, Annot),
	    memberchk(resource, Annot)
	->  IsResource = true
	;   true
	).
peval(Resource, Resource, true) :-
	atom(Resource).


		 /*******************************
		 *	SUBQUERY EVALUATION	*
		 *******************************/

%%	sparql_subquery(+Proj, :Query, +Solutions) is nondet.
%
%	Execute a SPARQL subquery.
%
%	@param	Proj is a list of variables that are shared with the
%		outer query.
%	@tbd	Call the optimizer.
%	@tbd	Sub queries must be evaluated before the outer query,
%		so we must move them to the head of the query
%		evaluation.  Not doing so causes no harm, but leads
%		to repetitive execution of the subquery.

sparql_subquery(Proj, Query, Solutions) :-
	vars_in_bindings(Proj, Vars),
	Reply =.. [row|Vars],
	sparql:select_results(Solutions, Reply, Query),
	debug(sparql(subquery), 'SubQuery result: ~q', [Proj]),
	unify_projection(Proj).

vars_in_bindings([], []).
vars_in_bindings([_Outer=Var|T0], [Var|T]) :-
	vars_in_bindings(T0, T).


unify_projection([]).
unify_projection([V=V|T]) :-
	unify_projection(T).


		 /*******************************
		 *	      UPDATE		*
		 *******************************/

%%	sparql_update(:Updates) is det.
%
%	Handle SPARQL update requests.
%
%	@tbd	Realise authorization rules

sparql_update(Module:Updates) :-
	rdf_transaction(update(Updates, Module), 'SPARQL').

update([], _).
update([H|T], M) :-
	update(H, M),
	update(T, M).
update(insert_data(Quads), _) :-
	maplist(insert_triple(user), Quads).
update(delete_data(Quads), _) :-
	maplist(delete_triple(user), Quads).
update(delete_where(Quads), _):-
	maplist(delete_triples(user), Quads).
update(add(_Silent, From, To), _) :-	% TBD: Error of From does not exist
	db(From, FromDB),
	db(To, ToDB),
	forall(rdf(S,P,O,FromDB:Line),
	       rdf_assert(S,P,O,ToDB:Line)).
update(move(_Silent, From, To), _) :-
	db(From, FromGraph),
	db(To, ToGraph),
	rdf_retractall(_,_,_,ToGraph),
	forall(rdf(S,P,O,FromGraph:Line),
	       ( rdf_retractall(S,P,O,FromGraph:Line),
		 rdf_assert(S,P,O,ToGraph:Line)
	       )).
update(copy(_Silent, From, To), _) :-
	db(From, FromGraph),
	db(To, ToGraph),
	rdf_retractall(_,_,_,ToGraph),
	forall(rdf(S,P,O,FromGraph:Line),
	       rdf_assert(S,P,O,ToGraph:Line)).
update(modify(With, Modify, _Using, Query), Module) :-
	db(With, Graph),
	forall(Module:Query,
	       modify(Modify, Graph)).
update(load(_Silent, URI, Into), _) :-
	(   Into = graph(Graph)
	->  rdf_load(URI, [graph(Graph)])
	;   rdf_load(URI)
	).
update(clear(_Silent, Clear), _) :-
	clear_db(Clear).

db(default, user).
db(graph(G), G).

modify(delete(Delete), Graph) :-
	maplist(delete_triple(Graph), Delete).
modify(insert(Insert), Graph) :-
	maplist(insert_triple(Graph), Insert).
modify(replace(Delete, Insert), Graph) :-
	maplist(delete_triple(Graph), Delete),
	maplist(insert_triple(Graph), Insert).

%%	insert_triple(+Graph, +Triple) is det.

insert_triple(Graph, rdf(S0,P,O0)) :- !,
	modify_subject(S0, S),
	modify_object(O0, O),
	rdf_assert(S,P,O, Graph).
insert_triple(_, rdf(S0,P,O0,G0)) :-
	graph(G0, G),
	modify_subject(S0, S),
	modify_object(O0, O),
	rdf_assert(S,P,O,G).

%%	delete_triple(+Graph, +Triple) is det.
%
%	Delete matching triples

delete_triple(Graph, rdf(S,P,O0)) :- !,
	modify_object(O0, O),
	rdf_retractall(S,P,O,Graph).
delete_triple(_, rdf(S,P,O0,G0)) :- !,
	graph(G0, G),
	modify_object(O0, O),
	rdf_retractall(S,P,O,G).

%%	delete_triples(+Graph:atom, +SimpleTriplePattern:compound) is det.

delete_triples(G0, Triple):-
	(   Triple = rdf(S,P,O),
	    G = G0
	;   Triple = rdf(S,P,O,G)
	),
	forall(
	    rdf(S,P,O),
	    delete_triple(G, rdf(S,P,O))
	).

modify_subject(bnode(Id), BNode) :- !,
	id_to_bnode(Id, BNode).
modify_subject(S, S).

modify_object(literal(_Q,V), literal(V)) :- !.
modify_object(bnode(Id), BNode) :- !,
	id_to_bnode(Id, BNode).
modify_object(O, O).

id_to_bnode(Id, BNode):-
	(   bnode_store(Id, BN)
	->  BN = BNode
	;   rdf_bnode(BN),
	    asserta(bnode_store(Id, BN))
	->  BN = BNode
	).

%%	graph(+Spec, -Graph)

graph(G:L, Graph) :-
	atom(G), !,
	(   integer(L)
	->  Graph = (G:L)
	;   Graph = G
	).
graph(G, G).

%%	clear_db(+Clear)
%
%	Note that CLEAR ALL cannot use rdf_reset_db  because we are in a
%	transaction.

clear_db(all) :-
	rdf_retractall(_,_,_).
clear_db(default) :-
	rdf_retractall(_,_,_,user).
clear_db(named) :-
	forall((rdf_graph(Graph), Graph \== user),
	       rdf_retractall(_,_,_,Graph)).
clear_db(graph(Graph)) :-
	rdf_retractall(_,_,_,Graph).
