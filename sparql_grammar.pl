/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007, University of Amsterdam

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

:- module(sparql_grammar,
	  [ sparql_parse/3		% +In, -Query, Options
	  ]).
:- use_module(library('semweb/rdf_db')).
:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(url)).
:- use_module(library(option)).
:- use_module(library(record)).

%%	sparql_parse(+SPARQL, -Query, +Options)
%
%	Parse the SPARQL statement Input   into a Prolog representation.
%	Based on "SPARQL Query Language for RDF", April 6, 2006. Options
%	supported:
%
%		* base_uri(+Base)
%		Base used if there is no BASE clause in the query

sparql_parse(Codes, Query, Options) :-
	is_list(Codes), !,
	(   phrase(sparql_query(Prolog, Query0), Codes)
	->  true
	;   syntax_error(unknown)
	),
	resolve_names(Prolog, Query0, Query, Options).
sparql_parse(Atomic, Query, Options) :-
	atomic(Atomic), !,
	atom_codes(Atomic, Codes),
	sparql_parse(Codes, Query, Options).
sparql_parse(Input, _, _) :-
	throw(error(type_error(text, Input), _)).


		 /*******************************
		 *	       ERRORS		*
		 *******************************/

syntax_error(What) :-
	throw(error(syntax_error(What),
		    context(_, 'in SPARQL query'))).

syntax_error(What, In, []) :-
	throw(error(syntax_error(What),
		    context(_, left(In)))).

add_error_location(error(syntax_error(What),
			 context(_, left(After))),
		   Input) :-
	append(Before, After, Input),
	length(Before, BL),
	CLen = 80,
	(   BL =< CLen
	->  BC = Before
	;   length(BC0, CLen),
	    append(_, BC0, Before),
	    append("...", BC0, BC)
	),
	length(After, AL),
	(   AL =< CLen
	->  AC = After
	;   length(AC0, CLen),
	    append(AC0, _, After),
	    append(AC0, "...", AC)
	),
	append("\n**here**\n", AC, HAC),
	append([0'\n|BC], HAC, ContextCodes),	% '
	atom_codes(Context, ContextCodes),
	throw(error(syntax_error(What),
		    context('SPARQL', Context))).


		 /*******************************
		 *	      RESOLVE		*
		 *******************************/

:- record
	state(base_uri,
	      prefix_assoc,
	      var_assoc,
	      var_list=[],
	      graph=[]).

%%	resolve_names(+Prolog, +Query0, -Query, +Options)
%
%	Turn var(Name) into Prolog variables and resolve all IRIs to
%	absolute IRIs.

resolve_names(Prolog, Q0, Q, Options) :-
	resolve_state(Prolog, State0, Options),
	resolve(Q0, Q, State0).

resolve(select(Proj0, DataSets0, Q0, Solutions0),
	select(Proj,  DataSets,  Q,  Solutions),
	State0) :-
	resolve_datasets(DataSets0, DataSets, State0),
	resolve_query(Q0, Q1, State0, State1),
	resolve_projection(Proj0, Proj, State1, State2),
	resolve_solutions(Solutions0, Solutions, Q2, State2),
	mkconj(Q1, Q2, Q).
resolve(construct(Templ0, DataSets0, Q0, Solutions0),
	construct(Templ,  DataSets,  Q,  Solutions),
	State0) :-
	resolve_datasets(DataSets0, DataSets, State0),
	resolve_query(Q0, Q1, State0, State1),
	resolve_construct_template(Templ0, Templ, State1, State2),
	resolve_solutions(Solutions0, Solutions, Q2, State2),
	mkconj(Q1, Q2, Q).
resolve(ask(DataSets0, Q0), ask(DataSets, Q), State0) :-
	resolve_datasets(DataSets0, DataSets, State0),
	resolve_query(Q0, Q, State0, _).
resolve(describe(Proj0, DataSets0, Q0, Solutions0),
	describe(Proj,  DataSets,  Q,  Solutions),
	State0) :-
	resolve_datasets(DataSets0, DataSets, State0),
	resolve_query(Q0, Q1, State0, State1),
	resolve_projection(Proj0, Proj, State1, State2),
	resolve_solutions(Solutions0, Solutions, Q2, State2),
	mkconj(Q1, Q2, Q).

%%	resolve_datasets(+Raw, -IRIs, +State)
%
%	TBD: what is the difference between named and non-named?

resolve_datasets([], [], _).
resolve_datasets([H0|T0], [H|T], S) :-
	resolve_dataset(H0, H, S),
	resolve_datasets(T0, T, S).

resolve_dataset(T0, IRI, S) :-
	resolve_iri(T0, IRI, S).

%%	resolve_query(+Q0, -Q, +State0, -State)
%
%	Create the initial translation from the  output of the parser to
%	a Prolog query.   Constructs in the output are:
%
%	    * (Qa,Qb)
%	    * (Qa;Qb)
%	    * (Q*->true;true)
%	    * rdf(S,P,O)
%	    * rdf(S,P,O,G:_).
%	    * sparql_true(Expression)
%	    * sparql_eval(Expression, Value)
%
%	Note that an rdf/3 object can  be literal(plain(X), X) to demand
%	an unqualified literal.

resolve_query(List, Q, S0, S) :-
	is_list(List), !,
	list_to_conj(List, Q, S0, S).
resolve_query((A0,B0), Q, S0, S) :- !,
	resolve_query(A0, A, S0, S1),
	resolve_query(B0, B, S1, S),
	mkconj(A, B, Q).
resolve_query((A0;B0), (A;B), S0, S) :- !,
	resolve_query(A0, A, S0, S1),
	resolve_query(B0, B, S1, S).
resolve_query(optional(true), true, S, S) :- !.
resolve_query(optional(Q0), (Q *-> true ; true), S0, S) :- !,
	resolve_query(Q0, Q, S0, S).
resolve_query(rdf(Subj0,P0,O0), RDF, S0, S) :- !,
	resolve_graph_term(Subj0, Subj, S0, S1),
	resolve_graph_term(P0, P, S1, S2),
	resolve_graph_term(O0, O, S2, S),
	rdf_goal(Subj, P, O, RDF, S).
resolve_query(graph(G0, Q0), Q, S0, S) :- !,
	resolve_graph_term(G0, G, S0, S1),
	state_graph(S1, GL),
	set_graph_of_state([G|GL], S1, S2),
	resolve_query(Q0, Q, S2, S3),
	set_graph_of_state(GL, S3, S).
resolve_query(Function, Call, S0, S) :-
	resolve_function(Function, Call, S0, S), !.
resolve_query(ebv(E0), sparql_true(E), S0, S) :- !,
	resolve_expression(E0, E, S0, S).
resolve_query(Q, Q, S, S).		% TBD

rdf_goal(S, P, O0, RDF, State) :-
	rdf_goal_object(O0, O),
	(   state_graph(State, [Graph|_])
	->  RDF = rdf(S, P, O, Graph:_)
	;   RDF = rdf(S, P, O)
	).

%%	rdf_goal_object(+ObjIn, -ObjGoal) is det.
%
%	Note that in SPARQL plain literals   (e.g.,  "hello") only match
%	literals that have neither a language  nor a type-qualifier. The
%	SemWeb library introduced rdf(S,P,literal(plain(X), X)) for this
%	purpose.

rdf_goal_object(O, O) :-
	var(O), !.
:- if((rdf_version(X), X >= 20800)).
rdf_goal_object(literal(X), O) :-
	atom(X), !,
	O = literal(plain(X), X).
:- endif.
rdf_goal_object(O, O).


mkconj(true, Q, Q) :- !.
mkconj(Q, true, Q) :- !.
mkconj(A, B, (A,B)).

list_to_conj([], true, S, S) :- !.
list_to_conj([Q0], Q, S0, S) :- !,
	resolve_query(Q0, Q, S0, S).
list_to_conj([H|T], (QH,QT), S0, S) :-
	resolve_query(H, QH, S0, S1),
	list_to_conj(T, QT, S1, S).

%%	resolve_projection(+Proj0, -VarList, +State0, State)
%
%	Return actual projection as a list of Name=Var

resolve_projection(*, Vars, State, State) :- !,
	state_var_list(State, Vars0),
	reverse(Vars0, Vars).
resolve_projection(VarNames, Vars, State, State) :-
	proj_vars(VarNames, Vars, State).

proj_vars([], [], _).
proj_vars([var(Name)|T0], [Name=Var|T], State) :- !,
	state_var_assoc(State, Assoc),
	(   get_assoc(Name, Assoc, Var)
	->  true
	;   Var = '$null$'			% or error?
	),
	proj_vars(T0, T, State).
proj_vars([IRI0|T0], [IRI|T], State) :-		% for DESCRIBE queries
	resolve_iri(IRI0, IRI, State),
	proj_vars(T0, T, State).

%%	resolve_construct_template(+Templ0, -Templ, +State)
%
%	Deal with ORDER BY clause.

resolve_construct_template([], [], S, S).
resolve_construct_template([H0|T0], [H|T], S0, S) :-
	resolve_construct_triple(H0, H, S0, S1),
	resolve_construct_template(T0, T, S1, S).

resolve_construct_triple(rdf(S0,P0,O0), rdf(S,P,O), St0, St) :-
	resolve_graph_term(S0, S, St0, St1),
	resolve_graph_term(P0, P, St1, St2),
	resolve_graph_term(O0, O, St2, St).

%%	resolve_solutions(+Solutions0, -Solutions, -Goal, +State)
%
resolve_solutions(distinct(S0), distinct(S), Goal, State) :- !,
	resolve_solutions(S0, S, Goal, State).
resolve_solutions(reduced(S0), reduced(S), Goal, State) :- !,
	resolve_solutions(S0, S, Goal, State).
resolve_solutions(solutions(unsorted, Limit, Offset),
		  solutions(unsorted, Limit, Offset), true, _) :- !.
resolve_solutions(solutions(order_by(OrderBy0), Limit, Offset),
		  solutions(order_by(OrderBy),  Limit, Offset),
		  Goal, State) :-
	resolve_order_by_cols(OrderBy0, OrderBy, Goal, State).

resolve_order_by_cols([], [], true, _).
resolve_order_by_cols([H0|T0], [H|T], Goal, State) :-
	resolve_order_by_col(H0, H, G0, State),
	resolve_order_by_cols(T0, T, G1, State),
	mkconj(G0, G1, Goal).

resolve_order_by_col(ascending(O0), ascending(O), Goal, State) :- !,
	compile_expression(O0, O, Goal, State).
resolve_order_by_col(decending(O0), decending(O), Goal, State) :- !,
	compile_expression(O0, O, Goal, State).

%%	resolve_state(+Prolog, -State)
%
%	Create initial state.

resolve_state(prolog(PrefixesList), State, Options) :-
	option(base_uri(Base), Options, 'http://default.base.org'),
	resolve_state(prolog(Base, PrefixesList), State, Options).
resolve_state(prolog(Base, PrefixesList),
	      State, _Options) :-
	list_to_assoc(PrefixesList, Prefixes),
	empty_assoc(Vars),
	make_state([ base_uri(Base),
		     prefix_assoc(Prefixes),
		     var_assoc(Vars)
		   ], State).

%%	resolve_graph_term(+T0, -T, +State0, -State) is det.

resolve_graph_term(Var, Var, S, S) :-
	var(Var), !.
resolve_graph_term(var(Name), Var, S0, S) :- !,
	resolve_var(Name, Var, S0, S).
resolve_graph_term(T, IRI, S, S) :-
	resolve_iri(T, IRI, S), !.
resolve_graph_term(literal(type(IRI0, Value)),
		   literal(type(IRI, Value)), S, S) :- !,
	resolve_iri(IRI0, IRI, S).
resolve_graph_term(boolean(Val),
		   literal(type(Type, Val)), S, S) :- !,
	rdf_equal(Type, xsd:boolean).
resolve_graph_term(T, T, S, S).


%%	resolve_expression(+E0, -E, +State0, -State)

resolve_expression(Var, Var, S, S) :-
	var(Var), !.
resolve_expression(or(A0,B0), or(A,B), S0, S) :- !,
	resolve_expression(A0, A, S0, S1),
	resolve_expression(B0, B, S1, S).
resolve_expression(and(A0,B0), and(A,B), S0, S) :- !,
	resolve_expression(A0, A, S0, S1),
	resolve_expression(B0, B, S1, S).
resolve_expression(regex(V0,P0,F0), regex(V,P,F), S0, S) :- !,
	resolve_expression(V0, V, S0, S1),
	resolve_expression(P0, P, S1, S2),
	resolve_expression(F0, F, S2, S).
resolve_expression(E0, E, S0, S) :-
	expression_op(E0), !,
	E0 =.. [Op|Args0],
	resolve_expressions(Args0, Args, S0, S),
	E =.. [Op|Args].
resolve_expression(E0, E, S0, S) :-
	resolve_function(E0, E, S0, S), !.
resolve_expression(T0, T, S0, S) :-
	resolve_graph_term(T0, T, S0, S). 	% OK?

expression_op(_ = _).
expression_op(_ \= _).			% SPARQL !=
expression_op(_ =< _).			% SPARQL <=
expression_op(_ >= _).
expression_op(_ < _).
expression_op(_ > _).
expression_op(_ + _).
expression_op(_ - _).
expression_op(_ * _).
expression_op(_ / _).
expression_op(not(_)).			% SPARQL !(_)
expression_op(+ _).
expression_op(- _).


resolve_expressions([], [], S, S).
resolve_expressions([H0|T0], [H|T], S0, S) :-
	resolve_expression(H0, H, S0, S1),
	resolve_expressions(T0, T, S1, S).

resolve_function(function(F0, Args0), function(Term), S0, S) :- !,
	resolve_iri(F0, F, S0),
	resolve_expressions(Args0, Args, S0, S),
	Term =.. [F|Args].
resolve_function(Builtin, Term, S0, S) :- !,
	built_in_function(Builtin), !,
	Builtin =.. [F|Args0],
	resolve_expressions(Args0, Args, S0, S),
	Term =.. [F|Args].

built_in_function(str(_)).
built_in_function(lang(_)).
built_in_function(langmatches(_,_)).
built_in_function(datatype(_)).
built_in_function(bound(_)).
built_in_function(isiri(_)).
built_in_function(isuri(_)).
built_in_function(isblank(_)).
built_in_function(isliteral(_)).


%%	resolve_var(+Name, -Var, +State0, ?State)
%
%	Resolve a variable. If State0 ==  State   and  it concerns a new
%	variable the variable is bound to '$null$'.

resolve_var(Name, Var, State, State) :-
	state_var_assoc(State, Vars),
	get_assoc(Name, Vars, Var), !.
resolve_var(Name, Var, State0, State) :- !,
	state_var_assoc(State0, Vars0),
	state_var_list(State0, VL),
	put_assoc(Name, Vars0, Var, Vars),
	set_var_assoc_of_state(Vars, State0, State1),
	set_var_list_of_state([Name=Var|VL], State1, State).
resolve_var(_, '$null$', State, State).

%%	resolve_iri(+Spec, -IRI:atom, +State) is det.
%
%	Translate Spec into a fully expanded IRI as used in RDF-DB. Note
%	that we must expand %xx sequences here.

resolve_iri(P:N, IRI, State) :- !,
	resolve_prefix(P, Prefix, State),
	url_iri(N, LocalIRI),
	atom_concat(Prefix, LocalIRI, IRI).
resolve_iri(URL0, IRI, State) :-
	atom(URL0),
	state_base_uri(State, Base),	% TBD: What if there is no base?
	global_url(URL0, Base, URL1),
	url_iri(URL1, IRI).

resolve_prefix(P, IRI, State) :-
	state_prefix_assoc(State, Prefixes),
	(   get_assoc(P, Prefixes, IRI)
	->  true
	;   rdf_db:ns(P, IRI)		% Extension: database known
	->  true
	;   throw(error(existence_error(prefix, P), _))
	).


%%	resolve_bnodes(+Pattern0, -Pattern)
%
%	Blank nodes are scoped into a   basic graph pattern (i.e. within
%	{...}). The code below  does  a   substitution  of  bnode(X)  to
%	variables in an arbitrary term.

resolve_bnodes(P0, P) :-
	empty_assoc(BN0),
	resolve_bnodes(P0, P, BN0, _).

resolve_bnodes(Var, Var, BN, BN) :-
	var(Var), !.
resolve_bnodes(bnode(Name), Var, BN0, BN) :- !,
	(   get_assoc(Name, BN0, Var)
	->  BN = BN0
	;   put_assoc(Name, BN0, Var, BN)
	).
resolve_bnodes(Term0, Term, BN0, BN) :-
	compound(Term0), !,
	functor(Term0, F, A),
	functor(Term, F, A),
	resolve_bnodes_args(0, A, Term0, Term, BN0, BN).
resolve_bnodes(Term, Term, BN, BN).

resolve_bnodes_args(A, A, _, _, BN, BN) :- !.
resolve_bnodes_args(I0, A, T0, T, BN0, BN) :-
	I is I0 + 1,
	arg(I, T0, A0),
	resolve_bnodes(A0, A1, BN0, BN1),
	arg(I, T, A1),
	resolve_bnodes_args(I, A, T0, T, BN1, BN).


		 /*******************************
		 *	COMPILE EXPRESSIONS	*
		 *******************************/

%%	compile_expression(+Expression, -Var, -Var, -Goal, +State)
%
%	Compile an expression into a (compound)   goal that evaluates to
%	the variable var. This version is  not realy compiling. Its just
%	the entry point for a future compiler.

compile_expression(Expr0, Var, Goal, State) :-
	resolve_expression(Expr0, Expr, State, State),
	(   primitive(Expr)
	->  Var = Expr,
	    Goal = true
	;   Goal = sparql_eval(Expr, Var)
	).

primitive(Var)  :- var(Var), !.
primitive(Atom) :- atom(Atom).		% IRI, '$null$'


		 /*******************************
		 *	    SPARQL DCG		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
From A.7. We keep the same naming and   order of the productions to make
it as easy as possible to verify the correctness of the parser.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	query(-Prolog, -Query)//

sparql_query(Prolog, Query, In, Out) :-
	catch(query(Prolog, Query, In, Out),
	      E,
	      add_error_location(E, In)).

query(Prolog, Query) -->
	skip_ws,
	prolog(Prolog),
	(   select_query(Query)
	;   construct_query(Query)
	;   describe_query(Query)
	;   ask_query(Query)
	), !.

%%	prolog(-Decls)//

prolog(prolog(Base, Prefixes)) -->
	base_decl(Base), !,
	prefix_decls(Prefixes, Base).
prolog(prolog(Prefixes)) -->
	prefix_decls(Prefixes, -).

prefix_decls([H|T], Base) -->
	prefix_decl(H, Base), !,
	prefix_decls(T, Base).
prefix_decls([], _) -->
	[].

%%	base_decl(-Base:uri)// is semidet.
%
%	Match "base <URI>"

base_decl(Base) -->
	keyword("base"),
	q_iri_ref(Base).

%%	prefix_decl(-Prefix, +Base)// is semidet.
%
%	Process "prefix <qname> <URI>" into a term Qname-IRI

prefix_decl(Id-IRI, Base) -->
	keyword("prefix"),
	(   qname_ns(Id),
	    q_iri_ref(IRI0)
	->  { global_url(IRI0, Base, IRI) }
	;   syntax_error(illegal_prefix_declaration)
	).

%%	select_query(-Select)// is semidet.
%
%	Process "select ..." into a term
%
% 	select(Projection, DataSets, Query, Solutions)

select_query(select(Projection, DataSets, Query, Solutions)) -->
	keyword("select"),
	(   keyword("distinct")
	->  { Solutions = distinct(S0) }
	;   keyword("reduced")
	->  { Solutions = reduced(S0) }
	;   { Solutions = S0 }
	),
	select_projection(Projection),
	data_sets(DataSets),
	where_clause(Query),
	solution_modifier(S0).

select_projection(*) --> "*", !, skip_ws.
select_projection([H|T]) -->
	var(H),
	vars(T), !.
select_projection(_) -->
	syntax_error(projection_expected).

vars([H|T]) -->
	var(H),
	vars(T).
vars([]) -->
	[].


%%	construct_query(-Construct)// is semidet.
%
%	Processes "construct ..." into a term
%
%	construct(Template, DataSets, Query, Solutions)

construct_query(construct(Template, DataSets, Query, Solutions)) -->
	keyword("construct"),
	construct_template(Template),
	data_sets(DataSets),
	where_clause(Query),
	solution_modifier(Solutions).

%%	describe_query(-Describe)// is semidet.
%
%	Processes "describe ..." into a term
%
%	describe(Projection, DataSets, Query, Solutions)

describe_query(describe(Projection, DataSets, Query, Solutions)) -->
	keyword("describe"),
	desc_projection(Projection),
	data_sets(DataSets),
	(where_clause(Query) -> [] ; {Query = true}),
	solution_modifier(Solutions).

desc_projection(*) --> "*", !, skip_ws.
desc_projection([H|T]) -->
	var_or_iri_ref(H), !,
	var_or_iri_refs(T).
desc_projection(_) -->
	syntax_error(projection_expected).

var_or_iri_refs([H|T]) -->
	var_or_iri_ref(H), !,
	var_or_iri_refs(T).
var_or_iri_refs([]) -->
	[].

%%	ask_query(Query)//

ask_query(ask(DataSets, Query)) -->
	keyword("ask"),
	data_sets(DataSets),
	where_clause(Query).

data_sets([H|T]) -->
	dataset_clause(H), !,
	data_sets(T).
data_sets([]) -->
	[].

%%	dataset_clause(-Src)//

dataset_clause(Src) -->
	keyword("from"),
	(   default_graph_clause(Src)
	->  []
	;   named_graph_clause(Src)
	).

%%	default_graph_clause(-Src)

default_graph_clause(Src) -->
	source_selector(Src).

%%	named_graph_clause(Graph)//

named_graph_clause(Src) -->
	keyword("named"),
	source_selector(Src).

%%	source_selector(-Src)//

source_selector(Src) -->
	iri_ref(Src).

%%	where_clause(-Pattern)//

where_clause(Pattern) -->
	keyword("where"), !,
	must_see_group_graph_pattern(Pattern).
where_clause(Pattern) -->
	group_graph_pattern(Pattern).

must_see_group_graph_pattern(Pattern) -->
	group_graph_pattern(Pattern), !.
must_see_group_graph_pattern(_) -->
	syntax_error(group_graph_pattern_expected).


%%	solution_modifier(-Solutions)// is det.
%
%	Processes order by, limit and offet clauses into a term
%
%		solutions(Order, Limit, Offset)

solution_modifier(Modifier) -->
	{ Modifier = solutions(Order, Limit, Offset) },
	( order_clause(Order)   -> [] ; { Order  = unsorted } ),
	limit_offset_clauses(Limit, Offset).

limit_offset_clauses(Limit, Offset) -->
	limit_clause(Limit), !,
	( offset_clause(Offset) -> [] ; { Offset = 0 } ).
limit_offset_clauses(Limit, Offset) -->
	offset_clause(Offset), !,
	( limit_clause(Limit)   -> [] ; { Limit  = inf } ).
limit_offset_clauses(inf, 0) --> [].


%%	order_clause(-Order)//

order_clause(order_by([H|T])) -->
	keyword("order"),
	(   keyword("by"),
	    order_condition(H),
	    order_conditions(T)
	->  ""
	;   syntax_error(illegal_order_clause)
	).

order_conditions([H|T]) -->
	order_condition(H), !,
	order_conditions(T).
order_conditions([]) -->
	[].

%%	order_condition(-Order)//

order_condition(ascending(Expr)) -->
	keyword("asc"), !,
	bracketted_expression(Expr).
order_condition(decending(Expr)) -->
	keyword("desc"), !,
	bracketted_expression(Expr).
order_condition(ascending(Value)) -->
	(   constraint(Value)
	;   var(Value)
	), !.


%%	limit_clause(-Limit)//

limit_clause(Limit) -->
	keyword("limit"),
	integer(Limit).


%%	offset_clause(Offset)//

offset_clause(Offset) -->
	keyword("offset"),
	integer(Offset).


%%	group_graph_pattern(P)//

group_graph_pattern(P) -->
	"{", skip_ws,
	(   graph_pattern(P0)
	->  (   "}"
	    ->  skip_ws
	    ;	syntax_error(expected('}'))
	    ),
	    { resolve_bnodes(P0, P) }
	;   syntax_error(graph_pattern_expected)
	).


%%	graph_pattern(P)//

graph_pattern(P) -->
	filtered_basic_graph_pattern(P1),
	(   graph_pattern_not_triples(P2)
	->  optional_dot,
	    graph_pattern(P3),
	    { P = (P1,P2,P3) }
	;   { P = P1 }
	).


%%	filtered_basic_graph_pattern(P)

filtered_basic_graph_pattern(P) -->
	(   block_of_triples(P1)
	->  ""
	;   {P1=true}
	),
	(   filter(C)
	->  optional_dot,
	    filtered_basic_graph_pattern(P2),
	    { P = (P1,C,P2) }
	;   { P = P1 }
	).

one_dot -->
	".", !, skip_ws,
	(   "."
	->  syntax_error("double_dot")
	;   ""
	).

optional_dot --> ".", skip_ws.
optional_dot --> "".

%%	block_of_triples(P)//
%
%	Looks the same to me??

block_of_triples(P) -->
	block_of_triples(P, []).

block_of_triples(List, T) -->
	triples_same_subject(List, T0),
	block_of_triples_cont(T0, T).

block_of_triples_cont(List, T) -->
	one_dot,
	triples_same_subject(List, T0), !,
	block_of_triples_cont(T0, T).
block_of_triples_cont(List, T) -->
	one_dot, !,
	block_of_triples_cont(List, T).
block_of_triples_cont(T, T) -->
	"".

%%	graph_pattern_not_triples(-Pattern)//

graph_pattern_not_triples(P) --> optional_graph_pattern(P), !.
graph_pattern_not_triples(P) --> group_or_union_graph_pattern(P), !.
graph_pattern_not_triples(P) --> graph_graph_pattern(P).

%%	optional_graph_pattern(Pattern)//

optional_graph_pattern(Pattern) -->
	keyword("optional"),
	must_see_group_graph_pattern(P0),
	{ Pattern = optional(P0) }.

%%	graph_graph_pattern(-Graph)// is semidet
%
%	Processes a "graph ..." clause into
%
%	graph(Graph, Pattern)

graph_graph_pattern(graph(Graph, Pattern)) -->
	keyword("graph"), !,
	var_or_blank_node_or_iri_ref(Graph),
	must_see_group_graph_pattern(Pattern).

%%	group_or_union_graph_pattern(-Pattern)//

group_or_union_graph_pattern(Pattern) -->
	group_graph_pattern(P0),
	add_union(P0, Pattern).

add_union(P0, (P0;P)) -->
	keyword("union"), !,
	must_see_group_graph_pattern(P1),
	add_union(P1, P).
add_union(P, P) -->
	[].


%%	filter(-Filter)//

filter(ebv(Exp)) -->
	keyword("filter"),
	(   constraint(Exp)
	->  ""
	;   syntax_error(filter_expected)
	).

%%	constraint(-Filter)//

constraint(Exp) -->
	(   bracketted_expression(Exp)
	->  []
	;   built_in_call(Exp)
	->  ""
	;   function_call(Exp)
	).

%%	function_call(-Function)// is semidet.
%
%	Processes <URI>(Arg ...) into function(IRI, Args)

function_call(function(F, Args)) -->
	iri_ref(F),
	arg_list(Args).

%%	arg_list(-List)//

arg_list(List) -->
	"(", skip_ws,
	(   expression(A0)
	->  arg_list_cont(As),
	    {List = [A0|As]}
	;   {List = []}
	),
	(   ")"
	->  []
	;   syntax_error(expression_expected)
	),
	skip_ws.

arg_list_cont([H|T]) -->
	",", !, skip_ws,
	must_see_expression(H),
	arg_list_cont(T).
arg_list_cont([]) -->
	[].

%%	construct_template(Triples)//

construct_template(Triples) -->
	"{", skip_ws, construct_triples(Triples), "}", !, skip_ws.
construct_template(_) -->
	syntax_error(construct_template_expected).

%%	construct_triples(-List)//

construct_triples(List) -->
	construct_triples(List, []).

construct_triples(List, T) -->
	triples_same_subject(List, T0), !,
	(   one_dot
	->  (   peek("}")
	    ->  { T = T0 }
	    ;   construct_triples(T0, T)
	    )
	;   { T = T0 }
	).
construct_triples(T, T) -->
	"".

%%	triples_same_subject(-List, ?Tail)//
%
%	Return list of rdf(S,P,O) from triple spec.

triples_same_subject(List, Tail) -->
	var_or_term(S), !,
	property_list_not_empty(L, List, T0),
	{ make_triples_same_subject(L, S, T0, Tail) }.
triples_same_subject(List, Tail) -->
	triples_node(S, List, T0),
	property_list(L, T0, T1),
	{ make_triples_same_subject(L, S, T1, Tail) }.

make_triples_same_subject([], _, T, T).
make_triples_same_subject([property(P,O)|TP], S, [rdf(S,P,O)|T0], T) :-
	make_triples_same_subject(TP, S, T0, T).

%%	property_list(-L, -Triples, ?TriplesTail)//

property_list(L, Triples, Tail) -->
	property_list_not_empty(L, Triples, Tail), !.
property_list([], Tail, Tail) --> [].

%%	property_list_not_empty(-L, -Triples, ?TriplesTail)//

property_list_not_empty(E, Triples, Tail) -->
	verb(P),
	must_see_object_list(OL, Triples, T0),
	{ mk_proplist(OL, P, E, T) },
	(   ";", skip_ws
	->  property_list(T, T0, Tail)
	;   { T = [],
	      Tail = T0
	    }
	).

mk_proplist([], _, T, T).
mk_proplist([O|OT], P, [property(P,O)|T0], T) :-
	mk_proplist(OT, P, T0, T).

%%	object_list(-L, -Triples, ?TriplesTail)//

object_list(List, Triples, Tail) -->
	object(H, Triples, T0),
	(   ",", skip_ws
	->  { List = [H|T] },
	    object_list(T, T0, Tail)
	;   { List = [H],
	      Tail = T0
	    }
	).

must_see_object_list(List, Triples, Tail) -->
	object_list(List, Triples, Tail), !.
must_see_object_list(_,_,_) -->
	syntax_error(object_list_expected).

object(Obj, Triples, Tail) -->
	graph_node(Obj, Triples, Tail).

%%	verb(-E)//

verb(E) --> var_or_iri_ref(E), !.
verb(E) --> "a", skip_ws, { rdf_equal(E, rdf:type) }.

%%	triples_node(-Subj, -Triples, ?TriplesTail)//

triples_node(Subj, Triples, Tail) -->
	collection(Subj, Triples, Tail), !.
triples_node(Subj, Triples, Tail) -->
	blank_node_property_list(Subj, Triples, Tail).

%%	blank_node_property_list(-Subj, -Triples, ?TriplesTail)//

blank_node_property_list(Subj, Triples, Tail) -->
	"[", skip_ws,
	property_list_not_empty(List, Triples, T0),
	"]", skip_ws,
	{ make_triples_same_subject(List, Subj, T0, Tail) }.

%%	collection(-Subj, -Triples, ?TriplesTail)//

collection(CollSubj, Triples, Tail) -->
	"(", skip_ws,
	graph_node(H, Triples, T0),
	graph_nodes(T, T0, T1),
	")", skip_ws,
	{ mkcollection([H|T], CollSubj, T1, Tail) }.

mkcollection([Last], S, [ rdf(S, rdf:first, Last),
			  rdf(S, rdf:rest, rdf:nil)
			| Tail
			], Tail) :- !.
mkcollection([H|T], S, [ rdf(S, rdf:first, H),
			 rdf(S, rdf:rest, R)
		       | RDF
		       ], Tail) :-
	mkcollection(T, R, RDF, Tail).

graph_nodes([H|T], Triples, Tail) -->
	graph_node(H, Triples, T0), !,
	graph_nodes(T, T0, Tail).
graph_nodes([], T, T) --> [].

%%	graph_node(E, -Triples, ?TriplesTail)//

graph_node(E, T, T)       --> var_or_term(E), !.
graph_node(E, Triples, T) --> triples_node(E, Triples, T).

%%	var_or_term(-E)//

var_or_term(E) --> var(E), !.
var_or_term(E) --> graph_term(E).

%%	var_or_iri_ref(-E)//

var_or_iri_ref(E) --> var(E), !.
var_or_iri_ref(E) --> iri_ref(E), !.

%%	var_or_blank_node_or_iri_ref(-E)//

var_or_blank_node_or_iri_ref(T) --> var(T), !.
var_or_blank_node_or_iri_ref(T) --> blank_node(T), !.
var_or_blank_node_or_iri_ref(T) --> iri_ref(T), !.

%%	var(-Var)//

var(var(Name)) -->
	(   var1(Name)
	->  []
	;   var2(Name)
	),
	skip_ws.

%%	graph_term(-T)//

graph_term(T)    --> iri_ref(T), !.
graph_term(T)    --> rdf_literal(T), !.
graph_term(T)    --> numeric_literal(T), !.
graph_term(T)    --> boolean_literal(T), !.
graph_term(T)	 --> blank_node(T).
graph_term(T)	 --> nil(T).


%%	expression(-E)//

expression(E) -->
	conditional_or_expression(E),
	skip_ws.

must_see_expression(E) -->
	expression(E), !.
must_see_expression(_) -->
	syntax_error(expression_expected).

%%	conditional_or_expression(-E)//

conditional_or_expression(E) -->
	conditional_and_expression(E0),
	or_args(E0, E).

or_args(E0, or(E0,E)) --> "||", !, skip_ws, value_logical(E1), or_args(E1, E).
or_args(E, E) --> [].

%%	conditional_and_expression(-E)//

conditional_and_expression(E) -->
	value_logical(E0),
	and_args(E0, E).

and_args(E0, and(E0,E)) --> "&&", !, skip_ws, value_logical(E1), and_args(E1, E).
and_args(E, E) --> [].


%%	value_logical(-E)//

value_logical(E) --> relational_expression(E).

%%	relational_expression(E)//

relational_expression(E) -->
	numeric_expression(E0),
	(   relational_op(Op)
	->  skip_ws,
	    numeric_expression(E1),
	    { E =.. [Op,E0,E1] }
	;   { E = E0 }
	).

relational_op(=) --> "=".
relational_op(\=) --> "!=".
relational_op(=<) --> "<=".
relational_op(>=) --> ">=".
relational_op(<) --> "<".
relational_op(>) --> ">".


%%	numeric_expression(-E)//

numeric_expression(E) -->
	additive_expression(E).

%%	additive_expression(-E)//

additive_expression(E) -->
	multiplicative_expression(E0),
	add_args(E0, E).

add_args(E0, E0+E) --> "+", !, skip_ws,
	multiplicative_expression(E1), add_args(E1, E).
add_args(E0, E0-E) --> "-", !, skip_ws,
	multiplicative_expression(E1), add_args(E1, E).
add_args(E, E) --> [].



%%	multiplicative_expression(-E)//

multiplicative_expression(E) -->
	unary_expression(E0),
	mult_args(E0, E).

mult_args(E0, E0*E) --> "*", !, skip_ws,
	unary_expression(E1), mult_args(E1, E).
mult_args(E0, E0/E) --> "/", !, skip_ws,
	unary_expression(E1), mult_args(E1, E).
mult_args(E, E) --> [].


%%	unary_expression(-E)//

unary_expression(not(E)) --> "!", skip_ws, primary_expression(E).
unary_expression(+(E))   --> "+", skip_ws, primary_expression(E).
unary_expression(-(E))   --> "-", skip_ws, primary_expression(E).
unary_expression(E)      -->      	 primary_expression(E).


%%	primary_expression(-E)//

primary_expression(E) --> bracketted_expression(E), !.
primary_expression(E) --> built_in_call(E), !.
primary_expression(E) --> iri_ref_or_function(E), !.
primary_expression(E) --> rdf_literal(E), !.
primary_expression(E) --> numeric_literal(E), !.
primary_expression(E) --> boolean_literal(E), !.
primary_expression(E) --> blank_node(E), !.
primary_expression(E) --> var(E), !.


%%	bracketted_expression(-E)//

bracketted_expression(E) -->
	"(", skip_ws, must_see_expression(E), ")", skip_ws.

%%	built_in_call(-Call)//

built_in_call(F) -->
	get_keyword(KWD),
	{ built_in_function(KWD, Types) },
	"(", skip_ws, arg_list(Types, Args), ")", skip_ws,
	{ F =.. [KWD|Args] }.
built_in_call(Regex) -->
	regex_expression(Regex).

built_in_function(str,	       [expression]).
built_in_function(lang,	       [expression]).
built_in_function(langmatches, [expression, expression]).
built_in_function(datatype,    [expression]).
built_in_function(bound,       [var]).
built_in_function(isiri,       [expression]).
built_in_function(isuri,       [expression]).
built_in_function(isblank,     [expression]).
built_in_function(isliteral,   [expression]).

arg_list([HT|TT], [HA|TA]) -->
	arg(HT, HA),
	arg_list_cont(TT, TA).

arg_list_cont([], []) -->
	[].
arg_list_cont([H|T], [A|AT]) -->
	",", skip_ws,
	arg(H, A),
	arg_list_cont(T, AT).

arg(expression, A) --> expression(A).
arg(var,        A) --> var(A).

%%	regex_expression(-Regex)//

regex_expression(regex(Target, Pattern, Flags)) -->
	keyword("regex"),
	"(", skip_ws,
	must_see_expression(Target), ",", skip_ws,
	must_see_expression(Pattern),
	(   ",", skip_ws, must_see_expression(Flags)
	->  []
	;   {Flags = literal('')}
	),
	")", skip_ws.

%%	iri_ref_or_function(-Term)//

iri_ref_or_function(Term) -->
	iri_ref(IRI),
	(   arg_list(Args)
	->  { Term = function(IRI, Args) }
	;   { Term = IRI }
	).

%%	rdf_literal(-Literal)//

rdf_literal(literal(Value)) -->
	string(String),
	(   langtag(Lang)
	->  { Value = lang(Lang, String) }
	;   "^^", iri_ref(IRI)
	->  { Value = type(IRI, String) }
	;   { Value = String }
	),
	skip_ws.

%%	numeric_literal(-Number)//
%
%	Match a literal value and return it as a term
%
%		literal(type(Type, Atom))
%
%	Where Type is one of xsd:double,  xsd:decimal or xsd:integer and
%	Atom is the matched text. The   value  cannot always be obtained
%	using atom_number/2 because floats and decimals can start or end
%	with a '.', something which is not allowed in Prolog.

numeric_literal(literal(type(Type, Value))) -->
	optional_pm(Codes, CV),
	(   double_string(CV)
	->  { rdf_equal(xsd:double, Type) }
	;   decimal_string(CV)
	->  { rdf_equal(xsd:decimal, Type) }
	;   integer_string(CV)
	->  { rdf_equal(xsd:integer, Type) }
	), !,
	{ atom_codes(Value, Codes)
	},
	skip_ws.

%%	boolean_literal(-TrueOrFalse)//

boolean_literal(Lit) -->
	(   keyword("true")
	->  { Lit = boolean(true) }
	;   keyword("false")
	->  { Lit = boolean(false) }
	).

%%	string(-Atom)//

string(Atom) --> string_literal_long1(Atom), !.
string(Atom) --> string_literal_long2(Atom), !.
string(Atom) --> string_literal1(Atom), !.
string(Atom) --> string_literal2(Atom).

%%	iri_ref(IRI)//

iri_ref(IRI) -->
	q_iri_ref(IRI).
iri_ref(IRI) -->
	qname(IRI).			% TBD: qname_ns also returns atom!?

%%	qname(-Term)//
%
%	TBD: Looks like this is ambiguous!?

qname(Term) -->
	'QNAME'(Term), !, skip_ws.
qname(Q:'') -->
	qname_ns(Q).

%%	blank_node(-Id)//
%
%	Blank node.  Anonymous blank nodes are returned with unbound Id

blank_node(Id) -->
	blank_node_label(Id), !.
blank_node(Id) -->
	anon(Id).

		 /*******************************
		 *	       BASICS		*
		 *******************************/

%%	q_iri_ref(-Atom)//

q_iri_ref(Atom) -->
	"<",
	(    q_iri_ref_codes(Codes), ">"
	->   skip_ws,
	     { atom_codes(Atom, Codes) }
	;    syntax_error(illegal_qualified_iri)
	).

q_iri_ref_codes([]) -->
	[].
q_iri_ref_codes([H|T]) -->
	iri_code(H), !,
	q_iri_ref_codes(T).
q_iri_ref_codes(_) -->
	syntax_error(illegal_code_in_iri).

iri_code(Code) -->
	[Code],
	{ \+ not_iri_code(Code) }, !.
iri_code(Code) -->
	uchar(Code).

not_iri_code(0'<).
not_iri_code(0'>).
not_iri_code(0'').
not_iri_code(0'{).
not_iri_code(0'}).
not_iri_code(0'|).
not_iri_code(0'\\).			% not sure!?
not_iri_code(0'`).
not_iri_code(Code) :- between(0x00, 0x20, Code).


%%	qname_ns(Q)//

qname_ns(Q) -->
	ncname_prefix(Q), ":", !, skip_ws.
qname_ns('') -->
	":", skip_ws.

%	'QNAME'(-Term)//
%
%	Qualified name.  Term is one of Q:N or '':N

'QNAME'(Q:N) -->
	ncname_prefix(Q), ":", !, pn_local(N).
'QNAME'('':N) -->
	":", pn_local(N).


%%	blank_node_label(-Bnode)// is semidet.
%
%	Processes "_:..." into a bnode(Name) term.

blank_node_label(bnode(Name)) -->
	"_:", pn_local(Name), skip_ws.


%%	var1(-Atom)// is semidet.
%%	var2(-Atom)// is semidet.

var1(Name) --> "?", varname(Name).
var2(Name) --> "$", varname(Name).


%%	langtag(-Tag)//
%
%	Return language tag (without leading @)

langtag(Atom) -->
	"@",
	one_or_more_ascii_letters(Codes, T0),
	sub_lang_ids(T0, []),
	skip_ws,
	{ atom_codes(Atom, Codes) }.

sub_lang_ids([0'-|Codes], Tail) -->
	"-", !,
	one_or_more_ascii_letter_or_digits(Codes, T0),
	sub_lang_ids(T0, Tail).
sub_lang_ids(T, T) -->
	[].


%%	integer(-Integer)// is semidet.
%
%	Match an integer and return its value.

integer(Integer) -->
	integer_string(Codes),
	{ number_codes(Integer, Codes)
	},
	skip_ws.


%%	integer_string(-Codes)// is semidet.
%
%	Extract integer value.

integer_string(Codes) -->
	one_or_more_digits(Codes, []), !.

%%	decimal_string(-Codes)//
%
%	Extract float without exponent and return  the matched text as a
%	list of codes.

decimal_string(Codes) -->
	one_or_more_digits(Codes, T0), !,
	dot(T0, T1),
	digits(T1, []).
decimal_string(Codes) -->
	dot(Codes, T1),
	one_or_more_digits(T1, []).


%%	double_string(-Codes)// is semidet.
%
%	Extract a float number with exponent and  return the result as a
%	list of codes.

double_string(Codes) -->
	one_or_more_digits(Codes, T0), !,
	dot(T0, T1),
	digits(T1, T2),
	exponent(T2, []).
double_string(Codes) -->
	dot(Codes, T1),
	one_or_more_digits(T1, T2), !,
	exponent(T2, []).
double_string(Codes) -->
	one_or_more_digits(Codes, T2), !,
	exponent(T2, []).

dot([0'.|T], T) --> ".".		% 0'


%%	exponent(-Codes, ?Tail)//
%
%	Float exponent.  Returned as difference-list

exponent(Codes, T) -->
	optional_e(Codes, T0),
	optional_pm(T0, T1),
	one_or_more_digits(T1, T).

optional_e([0'e|T], T) -->
	(   "e"
	;   "E"
	), !.
optional_e(T, T) -->
	"".

optional_pm([C|T], T) -->
	[C],
	{ C == 0'+ ; C == 0'- }, !.
optional_pm(T, T) -->
	"".

%%	string_literal1(-Atom)//

string_literal1(Atom) -->
	"'", !,
	string_literal_codes(Codes),
	"'", !,
	{ atom_codes(Atom, Codes) }.

%%	string_literal2(-Atom)//

string_literal2(Atom) -->
	"\"", !,
	string_literal_codes(Codes),
	"\"", !,
	{ atom_codes(Atom, Codes) }.

string_literal_codes([]) -->
	"".
string_literal_codes([H|T]) -->
	(   echar(H)
	;   uchar(H)
	;   [H], { \+ not_in_string_literal(H) }
	),
	string_literal_codes(T).

not_in_string_literal(0x5C).
not_in_string_literal(0x0A).
not_in_string_literal(0x0D).

%%	string_literal_long1(-Atom)//

string_literal_long1(Atom) -->
	"'''", !,
	string_literal_codes_long(Codes),
	"'''", !,
	{ atom_codes(Atom, Codes) }.

%%	string_literal_long2(-Atom)//

string_literal_long2(Atom) -->
	"\"\"\"", !,
	string_literal_codes_long(Codes),
	"\"\"\"", !,
	{ atom_codes(Atom, Codes) }.

string_literal_codes_long([]) -->
	"".
string_literal_codes_long([H|T]) -->
	(   echar(H)
	;   uchar(H)
	;   [H], { H \== 0'\\ }
	),
	string_literal_codes_long(T).


%%	echar(-Code)//
%
%	Escaped character

echar(Code) -->
	"\\", echar2(Code).

echar2(0'\t) --> "t".
echar2(0'\b) --> "b".
echar2(0'\n) --> "n".
echar2(0'\r) --> "r".
echar2(0'\f) --> "f".
echar2(0'\\) --> "\\".
echar2(0'")  --> "\"".
echar2(0'')  --> "'".

%%	uchar(-Code)//
%
%	\uXXXX or \UXXXXXXXX, returning character value

uchar(Code) -->
	"\\u", !,
	(   hex(D1), hex(D2), hex(D3), hex(D4)
	->  { Code is D1<<12 + D2<<8 + D3<<4 + D4 }
	;   syntax_error(illegal_uchar)
	).
uchar(Code) -->
	"\\U", !,
	(   hex(D1), hex(D2), hex(D3), hex(D4),
	    hex(D5), hex(D6), hex(D7), hex(D8)
	->  { Code is D1<<28 + D2<<24 + D3<<20 + D4<<16 +
	              D5<<12 + D6<<8 + D7<<4 + D8 }
	;   syntax_error(illegal_Uchar)
	).

%%	hex(-Weigth)//
%
%	HEX digit (returning numeric value)

hex(Weigth) -->
	[C],
	{ code_type(C, xdigit(Weigth)) }.


%%	nil(-NIL)//
%
%	End-of-collection (rdf:nil)

nil(NIL) --> "(", ws_star, ")", skip_ws, { rdf_equal(NIL, rdf:nil) }.

%	ws//
%
%	white space characters.

ws --> [0x20].
ws --> [0x09].
ws --> [0x0D].
ws --> [0x0A].

%	ws_star//

ws_star --> ws, !, ws_star.
ws_star --> "".

%	anon//
%
%	Anonymous resource

anon(bnode(_)) --> "[", ws_star, "]", skip_ws.


%%	pn_chars_base(-Code)//
%
%	Basic identifier characters

pn_chars_base(Code) -->
	esc_code(Code),
	{ pn_chars_base(Code) }, !.

pn_chars_base(Code) :- between(0'A, 0'Z, Code).
pn_chars_base(Code) :- between(0'a, 0'z, Code).
pn_chars_base(Code) :- between(0x00C0, 0x00D6, Code).
pn_chars_base(Code) :- between(0x00D8, 0x00F6, Code).
pn_chars_base(Code) :- between(0x00F8, 0x02FF, Code).
pn_chars_base(Code) :- between(0x0370, 0x037D, Code).
pn_chars_base(Code) :- between(0x037F, 0x1FFF, Code).
pn_chars_base(Code) :- between(0x200C, 0x200D, Code).
pn_chars_base(Code) :- between(0x2070, 0x218F, Code).
pn_chars_base(Code) :- between(0x2C00, 0x2FEF, Code).
pn_chars_base(Code) :- between(0x3001, 0xD7FF, Code).
pn_chars_base(Code) :- between(0xF900, 0xFDCF, Code).
pn_chars_base(Code) :- between(0xFDF0, 0xFFFD, Code).
pn_chars_base(Code) :- between(0x10000, 0xEFFFF, Code).

esc_code(Code) -->
	uchar(Code), !.
esc_code(Code) -->
	[ Code ].

%%	pn_chars_u(-Code)//
%
%	Allows for _

pn_chars_u(Code) -->
	esc_code(Code),
	{ pn_chars_u(Code) }.

pn_chars_u(Code) :-
	pn_chars_base(Code).
pn_chars_u(0'_).


%%	varname(-Atom)//
%
%	Name of a variable (after the ? or $)

varname(Atom) -->
	varchar1(C0),
	varchars(Cs),
	{ atom_codes(Atom, [C0|Cs]) },
	skip_ws.

varchar1(Code) -->
	esc_code(Code),
	{ varchar1(Code) }.

varchar1(Code) :-
	pn_chars_u(Code), !.
varchar1(Code) :-
	between(0'0, 0'9, Code).

varchars([H|T]) -->
	varchar(H), !,
	varchars(T).
varchars([]) -->
	[].

varchar(Code) -->
	esc_code(Code),
	{ varchar(Code) }.

varchar(Code) :-
	varchar1(Code), !.
varchar(Code) :-
	varchar_extra(Code), !.

varchar_extra(0x00B7).
varchar_extra(Code) :- between(0x0300, 0x036F, Code).
varchar_extra(Code) :- between(0x203F, 0x2040, Code).

ncchar(Code) :-
	varchar(Code), !.
ncchar(0'-).

%%	ncname_prefix(-Atom)//

ncname_prefix(Atom) -->
	pn_chars_base(C0),
	(   ncname_prefix_suffix(Cs)
	->  { atom_codes(Atom, [C0|Cs]) }
        ;   { char_code(Atom, C0) }
	).

ncname_prefix_suffix(Codes) -->
	ncchar_or_dots(Codes, []),
	{ \+ append(_, [0'.], Codes) }.

ncchar_or_dots([H|T0], T) -->
	ncchar_or_dot(H), !,
	ncchar_or_dots(T0, T).
ncchar_or_dots(T, T) -->
	[].

ncchar_or_dot(Code) -->
	esc_code(Code),
	{ ncchar_or_dot(Code) }.

ncchar_or_dot(Code) :-
	ncchar(Code), !.
ncchar_or_dot(0'.).

%%	pn_local(-Atom)//

pn_local(Atom) -->
	varchar1(C0),
	(   ncname_prefix_suffix(Cs)
	->  { atom_codes(Atom, [C0|Cs]) }
        ;   { char_code(Atom, C0) }
	).

		 /*******************************
		 *	      EXTRAS		*
		 *******************************/

digit(Code) -->
	[Code],
	{ between(0'0, 0'9, Code) }.

ascii_letter(Code) -->
	[Code],
	{ between(0'a, 0'z, Code)
	; between(0'A, 0'Z, Code)
	}, !.

ascii_letter_or_digit(Code) -->
	[Code],
	{ between(0'a, 0'z, Code)
	; between(0'A, 0'Z, Code)
	; between(0'0, 0'9, Code)
	}, !.

digits([H|T0], T) -->
	digit(H), !,
	digits(T0, T).
digits(T, T) -->
	[].

ascii_letters([H|T0], T) -->
	ascii_letter(H), !,
	ascii_letters(T0, T).
ascii_letters(T, T) -->
	[].

ascii_letter_or_digits([H|T0], T) -->
	ascii_letter_or_digit(H), !,
	ascii_letter_or_digits(T0, T).
ascii_letter_or_digits(T, T) -->
	[].

one_or_more_digits([C0|CT], Tail) -->
	digit(C0),
	digits(CT, Tail).

one_or_more_ascii_letters([C0|CT], Tail) -->
	ascii_letter(C0),
	ascii_letters(CT, Tail).

one_or_more_ascii_letter_or_digits([C0|CT], Tail) -->
	ascii_letter_or_digit(C0),
	ascii_letter_or_digits(CT, Tail).

%%	keyword(+Codes)
%
%	Case-insensitive match for a keyword.

keyword([]) -->
	(  ascii_letter(_)
	-> !, {fail}
	;  skip_ws
	).
keyword([H|T]) -->
	[C],
	{ code_type(H, to_lower(C)) },
	keyword(T).

%%	get_keyword(-Atom)
%
%	Get next identifier as lowercase

get_keyword(Atom) -->
	one_or_more_ascii_letters(Letters, []),
	{ atom_codes(Raw, Letters),
	  downcase_atom(Raw, Atom)
	},
	skip_ws.

%	skip_ws//

skip_ws -->
	ws, !,
	skip_ws.
skip_ws -->
	"#", !,
	skip_comment,
	skip_ws.
skip_ws -->
	[].

skip_comment --> "\n", !.
skip_comment --> "\r", !.
skip_comment --> eos, !.
skip_comment --> [_], skip_comment.

eos([], []).

peek(C, T, T) :-
	append(C, _, T), !.
