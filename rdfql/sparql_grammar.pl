/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007-2012, University of Amsterdam
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

:- module(sparql_grammar,
	  [ sparql_parse/3		% +In, -Query, +Options
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(uri)).
:- use_module(library(option)).
:- use_module(library(record)).
:- use_module(jena_properties).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(ordsets)).


/** <module> SPARQL Parser

@see SPARQL 1.1 specification
@see SPARQL test cases at http://www.w3.org/2009/sparql/docs/tests/
*/

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
	atom_codes('...', Elipsis),
	atom_codes('\n**here**\n', Here),
	(   BL =< CLen
	->  BC = Before
	;   length(BC0, CLen),
	    append(_, BC0, Before),
	    append(Elipsis, BC0, BC)
	),
	length(After, AL),
	(   AL =< CLen
	->  AC = After
	;   length(AC0, CLen),
	    append(AC0, _, After),
	    append(AC0, Elipsis, AC)
	),
	append(Here, AC, HAC),
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
	      graph=[],
	      filters=[],
	      aggregates=[]).

%%	resolve_names(+Prolog, +Query0, -Query, +Options)
%
%	Turn var(Name) into Prolog variables and resolve all IRIs to
%	absolute IRIs.

resolve_names(Prolog, Q0, Q, Options) :-
	resolve_state(Prolog, State0, Options),
	resolve(Q0, Q, State0, _State).

resolve(select(Proj0, DataSets0, Q0, Solutions0),
	select(Proj,  DataSets,  Q,  Solutions),
	State0, State) :-
	resolve_datasets(DataSets0, DataSets, State0),
	resolve_query(Q0, Q1, State0, State1),
	resolve_projection(Proj0, Proj, QExpr, State1, State2),
	resolve_solutions(Solutions0, Solutions, Q2, State2, State),
	mkconj(Q1, QExpr, Q12),
	mkconj(Q12, Q2, Q).
resolve(construct(Templ0, DataSets0, Q0, Solutions0),
	construct(Templ,  DataSets,  Q,  Solutions),
	State0, State) :-
	resolve_datasets(DataSets0, DataSets, State0),
	resolve_query(Q0, Q1, State0, State1),
	resolve_construct_template(Templ0, Templ, Q2, State1, State2),
	resolve_solutions(Solutions0, Solutions, Q3, State2, State),
	mkconj(Q1, Q2, Q12),
	mkconj(Q12, Q3, Q).
resolve(ask(DataSets0, Q0, Solutions0), ask(DataSets, Q, Solutions),
	State0, State) :-
	resolve_datasets(DataSets0, DataSets, State0),
	resolve_query(Q0, Q1, State0, State1),
	resolve_solutions(Solutions0, Solutions, Q2, State1, State),
	mkconj(Q1, Q2, Q).
resolve(describe(Proj0, DataSets0, Q0, Solutions0),
	describe(Proj,  DataSets,  Q,  Solutions),
	State0, State) :-
	resolve_datasets(DataSets0, DataSets, State0),
	resolve_query(Q0, Q1, State0, State1),
	resolve_projection(Proj0, Proj, QE, State1, State2),
	resolve_solutions(Solutions0, Solutions, Q2, State2, State),
	mkconj(Q1, QE, Q12),
	mkconj(Q12, Q2, Q).
resolve(update(Updates0), update(Updates), State0, State) :-
	resolve_updates(Updates0, Updates, State0, State).

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
resolve_query(group(G), Q, S0, S) :- !,
	state_filters(S0, FSave),
	set_filters_of_state([], S0, S1),
	resolve_query(G, Q0, S1, S2),
	state_filters(S2, Filters),
	set_filters_of_state(FSave, S2, S3),
	resolve_query(Filters, Q1, S3, S),
	mkconj(Q0, Q1, Q2),
	steadfast(Q2, Q).
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
resolve_query(rdf(Subj0,P0,O0), Q, S0, S) :-
	resolve_iri(P0, P1, S0),
	atom(P1),
	sparql:current_functional_property(P1, P, Argc), !,
	resolve_graph_term(Subj0, Subj, Q1, S0, S1),
	(   nonvar(O0),
	    O0 = collection(ArgList0),
	    resolve_graph_terms(ArgList0, ArgList, Q2, S1, S)
	->  true
	;   resolve_graph_term(O0, Arg, Q2, S1, S),
	    ArgList = [Arg]
	),
	FP =.. [P|ArgList],
	length(ArgList, ArgCount),
	(   ArgCount == Argc
	->  true
	;   throw(error(existence_error(functional_property, FP), _))
	),
	mkconj(Q1, Q2, Q12),
	FuncProp = sparql:functional_property(Subj, FP),
	mkconj(Q12, FuncProp, Q).
resolve_query(rdf(Subj,P,O), Q, S0, S) :- !,
	resolve_triple(Subj, P, O, Q, S0, S).
resolve_query(graph(G0, Q0), Q, S0, S) :- !,
	resolve_graph_term(G0, G, Q1, S0, S1),
	state_graph(S1, GL),
	set_graph_of_state([G|GL], S1, S2),
	resolve_query(Q0, Q2, S2, S3),
	mkconj(Q1, Q2, Q),
	set_graph_of_state(GL, S3, S).
resolve_query(Function, Q, S0, S) :-
	resolve_function(Function, Call, QF, S0, S), !,
	mkconj(QF, Call, Q).
resolve_query(ebv(E0), Q, S0, S) :- !,
	resolve_expression(E0, E, QE, S0, S),
	mkconj(QE, sparql_true(E), Q).
resolve_query(filter(E0), true, S0, S) :- !,
	state_filters(S0, F),
	set_filters_of_state([ebv(E0)|F], S0, S).
resolve_query(minus(QLeft0, QRight0), sparql_minus(QLeft, QRight), S0, S) :- !,
	resolve_query(QLeft0, QLeft, S0, S1),
	resolve_query(QRight0, QRight, S1, S).
resolve_query(bind(Expr0, var(VarName)), Q, S0, S) :- !,
	resolve_var(VarName, Var, S0, S1),
	state_aggregates(S1, A1),
	resolve_expression(Expr0, Expr, QE, S1, S2),
	state_aggregates(S2, A2),
	(   var(Expr)			% BIND(?var1 as ?var2)
	->  Var = Expr,
	    Q = rdfql_cond_bind_null([Var]),
	    S = S2
	;   A1 == A2
	->  mkconj(sparql_eval(Expr, Var), QE, Q),
	    S = S2
	;   Q = QE,
	    set_aggregates_of_state([sparql_eval(Expr, Var)|A2], S2, S)
	).
resolve_query(sub_select(Proj0, Q0, Sols0),
	      sparql_subquery(Proj, Q, Sols),
	      S0, S) :- !,
	subquery_state(S0, S1),
	resolve_query(Q0, Q1, S1, S2),
	resolve_projection(Proj0, Proj1, QExpr, S2, S3),
	resolve_solutions(Sols0, Sols, Q2, S3, _SubState),
	mkconj(Q1, QExpr, Q12),
	mkconj(Q12, Q2, Q),
	join_subquery_projection(Proj1, Proj, S0, S).
resolve_query(var_in(var(Name), Values0), member(Var, Values), S0, S) :-
	resolve_var(Name, Var, S0, S),
	resolve_values(Values0, Values, S).
resolve_query(vars_in(Vars0, Values0), member(Vars, Values), S0, S) :-
	resolve_vars(Vars0, Vars, S0, S),
	resolve_values_full(Values0, Values, S).
resolve_query(Q, Q, S, S).		% TBD

mkconj(true, Q, Q) :- !.
mkconj(Q, true, Q) :- !.
mkconj(A, B, (A,B)).

list_to_conj([], true, S, S) :- !.
list_to_conj([Q0], Q, S0, S) :- !,
	resolve_query(Q0, Q, S0, S).
list_to_conj([H|T], (QH,QT), S0, S) :-
	resolve_query(H, QH, S0, S1),
	list_to_conj(T, QT, S1, S).

mkdisj(true, _, true) :- !.
mkdisj(_, true, true) :- !.
mkdisj(A, B, (A;B)).


%%	resolve_projection(+Proj0, -VarList, -ExprQuery, +State0, State)
%
%	Return actual projection as a list of Name=Var
%
%	@param	ExprQuery is the query to resolve expressions that appear
%		in the projection.

resolve_projection(*, Vars, true, State, State) :- !,
	state_var_list(State, Vars0),
	reverse(Vars0, Vars).
resolve_projection(projection(VarNames, Bind), Vars, Q, State0, State) :-
	proj_vars(VarNames, Vars, State0, State1),
	resolve_query(Bind, Q, State1, State).

proj_vars([], [], State, State).
proj_vars([var(Name)|T0], [Name=Var|T], State0, State) :- !,
	resolve_var(Name, Var, State0, State1),
	proj_vars(T0, T, State1, State).
proj_vars([IRI0|T0], [IRI|T], State0, State) :-	% for DESCRIBE queries
	resolve_iri(IRI0, IRI, State0),
	proj_vars(T0, T, State0, State).

%%	resolve_construct_template(+Templ0, -Templ, -Q, +State)
%
%	Deal with ORDER BY clause.

resolve_construct_template([], [], true, S, S).
resolve_construct_template([H0|T0], [H|T], Q, S0, S) :-
	resolve_construct_triple(H0, H, Q1, S0, S1),
	resolve_construct_template(T0, T, Q2, S1, S),
	mkconj(Q1, Q2, Q).

resolve_construct_triple(rdf(S0,P0,O0), rdf(S,P,O), Q, St0, St) :-
	resolve_graph_term(S0, S, Q1, St0, St1),
	resolve_graph_term(P0, P, Q2, St1, St2),
	resolve_graph_term(O0, O, Q3, St2, St),
	mkconj(Q1, Q2, Q12),
	mkconj(Q12, Q3, Q).

%%	resolve_solutions(+Solutions0, -Solutions, -Q, +State0, -State)

resolve_solutions(distinct(S0), distinct(S), Q, State0, State) :- !,
	resolve_solutions(S0, S, Q, State0, State).
resolve_solutions(reduced(S0), reduced(S), Q, State0, State) :- !,
	resolve_solutions(S0, S, Q, State0, State).
resolve_solutions(solutions(Group0, Having0,      Order0, Limit, Offset),
		  solutions( Group,  Having,  Agg, Order, Limit, Offset),
		  Q, State0, State) :-
	resolve_group_by(Group0, Group, Q1, State0, State1),
	resolve_having(Having0, Having, Q2, State1, State2),
	resolve_order_by(Order0, Order, Q3, State2, State),
	state_aggregates(State, Agg),
	mkconj(Q1, Q2, Q12),
	mkconj(Q12, Q3, Q).


%%	resolve_order_by(+OrderBy0, -OrderBy, -Q, +State0, -State)

resolve_order_by(unsorted, unsorted, true, State, State).
resolve_order_by(order_by(Cols0), order_by(Cols), Q, State0, State) :-
	resolve_order_by_cols(Cols0, Cols, Q, State0, State).

resolve_order_by_cols([], [], true, State, State).
resolve_order_by_cols([H0|T0], [H|T], Q, State0, State) :-
	resolve_order_by_col(H0, H, Q1, State0, State1),
	resolve_order_by_cols(T0, T, Q2, State1, State),
	mkconj(Q1, Q2, Q).

resolve_order_by_col(ascending(O0), ascending(O), Goal, State0, State) :- !,
	compile_expression(O0, O, Goal, State0, State).
resolve_order_by_col(descending(O0), descending(O), Goal, State0, State) :- !,
	compile_expression(O0, O, Goal, State0, State).

%%	resolve_group_by(+Groups0, -Groups, -Q, +State0, -State)

resolve_group_by([], [], true, State, State).
resolve_group_by([H0|T0], [H|T], Q, State0, State) :-
	compile_expression(H0, H, Q1, State0, State1),
	resolve_group_by(T0, T, Q2, State1, State),
	mkconj(Q1, Q2, Q).

%%	resolve_having(+Having0, -Having, -Q, +State0, -State)

resolve_having(Having0, Having, true, State0, State) :-
	resolve_query(Having0, Having, State0, State).


%%	resolve_state(+Prolog, -State)
%
%	Create initial state.

resolve_state(prologue(PrefixesList), State, Options) :-
	option(base_uri(Base), Options, 'http://default.base.org/'),
	resolve_state(prologue(Base, PrefixesList), State, Options).
resolve_state(prologue(Base, PrefixesList),
	      State, _Options) :-
	list_to_assoc(PrefixesList, Prefixes),
	empty_assoc(Vars),
	make_state([ base_uri(Base),
		     prefix_assoc(Prefixes),
		     var_assoc(Vars)
		   ], State).

%%	resolve_graph_term(+T0, -T, -Q, +State0, -State) is det.

resolve_graph_term(Var, Var, true, S, S) :-
	var(Var), !.
resolve_graph_term(var(Name), Var, true, S0, S) :- !,
	resolve_var(Name, Var, S0, S).
resolve_graph_term(T, IRI, true, S, S) :-
	resolve_iri(T, IRI, S), !.
resolve_graph_term(literal(type(IRI0, Value)),
		   literal(type(IRI, Value)), true, S, S) :- !,
	resolve_iri(IRI0, IRI, S).
resolve_graph_term(boolean(Val),
		   literal(type(Type, Val)), true, S, S) :- !,
	rdf_equal(Type, xsd:boolean).
resolve_graph_term(collection(Members), CollSubj, Q, S0, S) :- !,
	mkcollection(Members, CollSubj, Triples, []),
	resolve_query(Triples, Q, S0, S).
resolve_graph_term(T, T, true, S, S).

%%	resolve_graph_terms(+TList0, -TList, -Q, +State0, -State) is det.

resolve_graph_terms([], [], true, S, S).
resolve_graph_terms([H0|T0], [H|T], Q, S0, S) :-
	resolve_graph_term(H0, H, Q1, S0, S1),
	resolve_graph_terms(T0, T, Q2, S1, S),
	mkconj(Q1, Q2, Q).

%%	resolve_triple(+Subj, +P, +O, -Q, +S0, -S).

resolve_triple(Subj0, P, O0, Q, S0, S) :-
	resolve_graph_term(Subj0, Subj, Q1, S0, S1),
	resolve_graph_term(O0, O, Q2, S1, S2),
	mkconj(Q1, Q2, Q12),
	resolve_path(P, Subj, O, Q3, S2, S),
	mkconj(Q12, Q3, Q).

%%	resolve_path(+P, +Subj, +Obj, -Q, +S0, -S) is det.
%
%	Translate a property path expression into a goal.
%
%	  - The argument of ! is a list of IRIs and ^(IRI)

resolve_path(P0, Subj, Obj, Q, S0, S) :-
	resolve_predicate(P0, P, S0, S), !,
	rdf_goal(Subj, P, Obj, Q, S).
resolve_path(P01/P02, Subj, Obj, Q, S0, S) :- !,
	resolve_path(P01, Subj, Tmp, Q1, S0, S1),
	resolve_path(P02, Tmp, Obj, Q2, S1, S),
	mkconj(Q1, Q2, Q).
resolve_path(^(P), Subj, Obj, Q, S0, S) :- !,
	resolve_path(P, Obj, Subj, Q, S0, S).
resolve_path(;(P01,P02), Subj, Obj, (Q1;Q2), S0, S) :- !,
	resolve_path(P01, Subj, Obj, Q1, S0, S),
	resolve_path(P02, Subj, Obj, Q2, S0, S).
resolve_path(!(NegSet0), Subj, Obj, Q, S, S) :- !,
	resolve_negated_property_set(NegSet0, NegSet, RevSet, S),
	rdf_goal(Subj, P, Obj, Q1, S),
	not_in_goal(P, NegSet, NotIn),
	(   RevSet == []
	->  Q = ( Q1, NotIn )
	;   rdf_goal(Obj, P2, Subj, Q2, S),
	    (	RevSet = [P2]
	    ->	RevNegate = Q2
	    ;	RevNegate = \+((Q2, memberchk(P2, RevSet)))
	    ),
	    (	NegSet == []
	    ->	Q = (Q1, RevNegate)
	    ;	Q = (Q1, NotIn, RevNegate)
	    )
	).
resolve_path(?(P), Subj, Obj, Q, S0, S) :- !,
	resolve_path(P, Subj, Obj, Q1, S0, S),
	Q = (Subj=Obj ; Q1).
resolve_path(*(P), Subj, Obj, Q, S0, S) :- !,
	resolve_path(P, From, To, Q1, S0, S),
	Q = sparql_find(Subj, Obj, From, To, Q1).
resolve_path(+(P), Subj, Obj, Q, S0, S) :- !,
	resolve_path(P, Subj, Tmp, Q1, S0, S),
	resolve_path(P, From, To, Q2, S0, S),
	Q = (Q1, sparql_find(Tmp, Obj, From, To, Q2)).


resolve_path(P, _, _, _, _, _) :-
	type_error(predicate_path, P).

%%	resolve_predicate(+P0, -P, +S0, -S) is det.

resolve_predicate(P, P, S, S) :-
	var(P), !.
resolve_predicate(var(Name), Var, S0, S) :- !,
	resolve_var(Name, Var, S0, S).
resolve_predicate(T, IRI, S, S) :-
	resolve_iri(T, IRI, S), !.

%%	resolve_negated_property_set(+PSet, -NegSet, -RevSet, +S) is det.
%
%	True when NegSet is the  set   of  forward negated properties in
%	PSet and RevSet is the seet of backward negated properties.

resolve_negated_property_set(PSet, NegSet, RevSet, S) :-
	resolve_netaged_property_set(PSet, NegSet, [], RevSet, [], S).

resolve_netaged_property_set((A0;B0), P0, P, N0, N, S) :- !,
	resolve_netaged_property_set(A0, P0, P1, N0, N1, S),
	resolve_netaged_property_set(B0, P1, P,  N1, N, S).
resolve_netaged_property_set(^(IRI0), P, P, [IRI|N], N, S) :-
	resolve_iri(IRI0, IRI, S).
resolve_netaged_property_set(IRI0, [IRI|P], P, N, N, S) :-
	resolve_iri(IRI0, IRI, S).

not_in_goal(P, [One], P \== One) :- !.
not_in_goal(P, List, \+ memberchk(P, List)).

%%	rdf_goal(+S, +P, +O, -RDF, +State)
%
%	Optionally add graph to the rdf/3 statement.

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
rdf_goal_object(literal(X), O) :-
	atom(X), !,
	O = literal(plain(X), X).
rdf_goal_object(O, O).


%%	mkcollection(+Members, -CollectionSubject, -Triples)

mkcollection([Last], S, [ rdf(S, rdf:first, Last),
			  rdf(S, rdf:rest, rdf:nil)
			| Tail
			], Tail) :- !.
mkcollection([H|T], S, [ rdf(S, rdf:first, H),
			 rdf(S, rdf:rest, R)
		       | RDF
		       ], Tail) :-
	mkcollection(T, R, RDF, Tail).

%%	resolve_expression(+E0, -E, -Q, +State0, -State)
%

resolve_expression(Var, Var, true, S, S) :-
	var(Var), !.
resolve_expression(or(A0,B0), or(A,B), Q, S0, S) :- !,
	resolve_expression(A0, A, Q1, S0, S1),
	resolve_expression(B0, B, Q2, S1, S),
	mkdisj(Q1, Q2, Q).
resolve_expression(and(A0,B0), and(A,B), Q, S0, S) :- !,
	resolve_expression(A0, A, Q1, S0, S1),
	resolve_expression(B0, B, Q2, S1, S),
	mkconj(Q1, Q2, Q).
resolve_expression(E0, E, Q, S0, S) :-
	expression_op(E0), !,
	E0 =.. [Op|Args0],
	resolve_expressions(Args0, Args, Q, S0, S),
	E =.. [Op|Args].
resolve_expression(E0, As, Q, S0, S) :-
	aggregate_op(E0), !,
	E0 =.. [Op|Args0],
	resolve_expressions(Args0, Args, Q, S0, S1),
	E =.. [Op|Args],
	state_aggregates(S0, A0),
	set_aggregates_of_state([aggregate(E,As)|A0], S1, S).
resolve_expression(E0, E, Q, S0, S) :-
	resolve_function(E0, E, Q, S0, S), !.
resolve_expression(exists(Pattern), boolean(True), Q, S0, S) :- !,
	resolve_query(Pattern, QE, S0, S),
	Q = (QE -> True=true ; True=false).
resolve_expression(in(E0, List0), in(E, List), Q, S0, S) :- !,
	resolve_expression(E0, E, Q1, S0, S1),
	resolve_expressions(List0, List, Q2, S1, S),
	mkconj(Q1, Q2, Q).
resolve_expression(not_in(E0, List0), not_in(E, List), Q, S0, S) :- !,
	resolve_expression(E0, E, Q1, S0, S1),
	resolve_expressions(List0, List, Q2, S1, S),
	mkconj(Q1, Q2, Q).
resolve_expression(not_exists(Pattern), boolean(True), Q, S0, S) :- !,
	resolve_query(Pattern, QE, S0, S),
	Q = (QE -> True=false ; True=true).
resolve_expression(distinct(E0), distinct(E), Q, S0, S) :- !,
	resolve_expression(E0, E, Q, S0, S).
resolve_expression(var(Name), Var, true, S0, S) :- !,
	resolve_var_invisible(Name, Var, S0, S).
resolve_expression(T0, T, Q, S0, S) :-
	resolve_graph_term(T0, T, Q, S0, S).	% OK?

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


resolve_expressions([], [], true, S, S).
resolve_expressions([H0|T0], [H|T], Q, S0, S) :-
	resolve_expression(H0, H, Q1, S0, S1),
	resolve_expressions(T0, T, Q2, S1, S),
	mkconj(Q1, Q2, Q).

resolve_function(function(F0, Args0), function(Term), Q, S0, S) :- !,
	resolve_iri(F0, F, S0),
	resolve_expressions(Args0, Args, Q, S0, S),
	Term =.. [F|Args].
resolve_function(concat(List0), concat(List), Q, S0, S) :- !,
	resolve_expressions(List0, List, Q, S0, S).
resolve_function(coalesce(List0), coalesce(List), Q, S0, S) :- !,
	resolve_expressions(List0, List, Q, S0, S).
resolve_function(uri(Expr0), iri(Expr, Base), Q, S0, S) :- !, % URI() == IRI()
	resolve_expression(Expr0, Expr, Q, S0, S),
	state_base_uri(S, Base).
resolve_function(iri(Expr0), iri(Expr, Base), Q, S0, S) :- !,
	resolve_expression(Expr0, Expr, Q, S0, S),
	state_base_uri(S, Base).
resolve_function(built_in(Builtin), built_in(Term), Q, S0, S) :- !,
	built_in_function(Builtin), !,
	Builtin =.. [F|Args0],
	resolve_expressions(Args0, Args, Q, S0, S),
	Term =.. [F|Args].
resolve_function(Builtin, Term, Q, S0, S) :- !,
	built_in_function(Builtin), !,
	Builtin =.. [F|Args0],
	resolve_expressions(Args0, Args, Q, S0, S),
	Term =.. [F|Args].

%%	resolve_var(+Name, -Var, +State0, ?State)
%
%	Resolve a variable. If State0 == State   and  it concerns a new
%	variable the variable is bound to '$null$'.

resolve_var(Name, Var, State0, State) :-
	assertion(atom(Name)),
	state_var_assoc(State0, Vars),
	get_assoc(Name, Vars, Visible-Var), !,
	(   Visible == true
	->  State = State0
	;   Visible = true,
	    state_var_list(State0, VL),
	    set_var_list_of_state([Name=Var|VL], State0, State)
	).
resolve_var(Name, Var, State0, State) :- !,
	state_var_assoc(State0, Vars0),
	state_var_list(State0, VL),
	put_assoc(Name, Vars0, true-Var, Vars),
	set_var_assoc_of_state(Vars, State0, State1),
	set_var_list_of_state([Name=Var|VL], State1, State).
resolve_var(_, '$null$', State, State).

%%	resolve_var_invisible(Name, -Var, +State0, ?State)
%
%	Similar to resolve_var/4, but does _not_ add the variable to the
%	set of variables visible in the projection if this is *.

resolve_var_invisible(Name, Var, State, State) :-
	assertion(atom(Name)),
	state_var_assoc(State, Vars),
	get_assoc(Name, Vars, _-Var), !.
resolve_var_invisible(Name, Var, State0, State) :- !,
	state_var_assoc(State0, Vars0),
	put_assoc(Name, Vars0, _-Var, Vars),
	set_var_assoc_of_state(Vars, State0, State).
resolve_var_invisible(_, '$null$', State, State).


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
	uri_normalized(URL0, Base, URL1),
	url_iri(URL1, IRI).

resolve_prefix(P, IRI, State) :-
	state_prefix_assoc(State, Prefixes),
	(   get_assoc(P, Prefixes, IRI)
	->  true
	;   rdf_db:ns(P, IRI)		% Extension: database known
	->  true
	;   throw(error(existence_error(prefix, P), _))
	).

%%	resolve_values(+Values0, -Values, +State) is det.
%
%	Resolve a list of values for the VALUES clause.

resolve_values([], [], _).
resolve_values([H0|T0], [H|T], S) :-
	resolve_value(H0, H, S),
	resolve_values(T0, T, S).

resolve_value(V0, V, S) :-
	resolve_graph_term(V0, V, Q, S, S2),
	assertion(Q == true),
	assertion(S2 == S).

resolve_values_full([], [], _).
resolve_values_full([H0|T0], [H|T], S) :-
	resolve_values(H0, H, S),
	resolve_values_full(T0, T, S).

resolve_vars([], [], S, S).
resolve_vars([var(Name)|T0], [V|T], S0, S) :-
	resolve_var(Name, V, S0, S1),
	resolve_vars(T0, T, S1, S).


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


%%	subquery_state(OuterState, SubState) is det.
%
%	Create an initial state for a subquery

subquery_state(S0, S) :-
	state_base_uri(S0, Base),
	state_prefix_assoc(S0, Prefixes),
	state_graph(S0, Graph),			% is this right?
	empty_assoc(Vars),
	make_state([ base_uri(Base),
		     prefix_assoc(Prefixes),
		     var_assoc(Vars),
		     graph(Graph)
		   ], S).

%%	join_subquery_projection(+Proj0, -Proj, +S0, -S) is det.
%
%	Link the projection variables of the   inner  query to the outer
%	query.
%
%	@param Proj is a list OuterVar=InnerVar

join_subquery_projection([], [], S, S).
join_subquery_projection([Name=InnerVar|T0], [OuterVar=InnerVar|T], S0, S) :-
	resolve_var(Name, OuterVar, S0, S1),
	join_subquery_projection(T0, T, S1, S).


%%	resolve_updates(+UpdatesIn, -UpdatesOut, +StateIn, -StateOut)
%
%	Resolve update requests. Each update is  expressed by one of the
%	following terms:
%
%	  - insert_data(+Quads)
%	  Insert Quads.  Quads is a list of rdf/3 or rdf/4 terms.
%	  - delete_data(+Quads)
%	  Delete Quads.  Quads is a list of rdf/3 or rdf/4 terms.
%	  - delete_where(+Quads)
%	  Delete Quads.  Quads is a list of rdf/3 or rdf/4 terms.
%	  - add(+Silent, +FromGraph, +ToGraph)
%	  Copy all triples from FromGraph to ToGraph
%	  - create(+Silent, +Graph)
%	  Create an empty graph
%	  - modify(WithIRI, +InsDel, +Using, -Query)
%	  - load(+Silent, +IRI, +Graph)

resolve_updates([], [], State, State).
resolve_updates([H0|T0], [H|T], State0, State) :-
	resolve_update(H0, H, State0, State1),
	resolve_updates(T0, T, State1, State).


resolve_update(insert_data(Quads0), insert_data(Quads), State0, State) :-
	resolve_quads(Quads0, Quads, State0, State).
resolve_update(delete_data(Quads0), delete_data(Quads), State0, State) :-
	resolve_quads(Quads0, Quads, State0, State).
resolve_update(delete_where(Quads0), delete_where(Quads), State0, State) :-
	resolve_quads(Quads0, Quads, State0, State).
resolve_update(add(Silent, From0, To0), add(Silent, From, To),
	       State, State) :-
	resolve_graph_or_special(From0, From, State),
	resolve_graph_or_special(To0, To, State).
resolve_update(copy(Silent, From0, To0), copy(Silent, From, To),
	       State, State) :-
	resolve_graph_or_special(From0, From, State),
	resolve_graph_or_special(To0, To, State).
resolve_update(move(Silent, From0, To0), move(Silent, From, To),
	       State, State) :-
	resolve_graph_or_special(From0, From, State),
	resolve_graph_or_special(To0, To, State).
resolve_update(create(Silent, Graph0), create(Silent, Graph), State, State) :-
	resolve_iri(Graph0, Graph, State).
resolve_update(modify(WithIRI0, InsDel0, Using0, Pattern),
	       modify(WithIRI,  InsDel,  Using,  Query),
	       State0, State) :-
	resolve_with(WithIRI0, WithIRI, State0),
	(   InsDel0 =.. [Action,Quads0]
	->  InsDel  =.. [Action,Quads],
	    resolve_quads(Quads0, Quads, State0, State2)
	;   InsDel0 = replace(DelQuads0, InsQuads0),
	    InsDel  = replace(DelQuads,  InsQuads),
	    resolve_quads(DelQuads0, DelQuads, State0, State1),
	    resolve_quads(InsQuads0, InsQuads, State1, State2)
	),
	Using0 = Using,
	resolve_query(Pattern, Query, State2, State).
resolve_update(drop(Silent, GraphAll0),
	       drop(Silent, GraphAll),
	       State, State) :-
	resolve_graph_or_special(GraphAll0, GraphAll, State).
resolve_update(clear(Silent, GraphAll0),
	       clear(Silent, GraphAll),
	       State, State) :-
	resolve_graph_or_special(GraphAll0, GraphAll, State).
resolve_update(load(Silent, IRI0, Graph0),
	       load(Silent, IRI,  Graph),
	       State, State) :-
	resolve_iri(IRI0, IRI, State),
	resolve_graph_or_special(Graph0, Graph, State).


%%	resolve_quads(+Quads, -Query, +State0, -State) is det.
%
%	This seems to be the same  as   resolve_query/4.  It  does a bit
%	more, but that should not harm us.  The output is a conjunction,
%	which we do not want, so we convert it back into a list.

resolve_quads(Quads0, Quads, State0, State) :-
	resolve_query(Quads0, Query, State0, State),
	phrase(query_quads(Query), Quads).

query_quads((A,B)) --> !,
	query_quads(A),
	query_quads(B).
query_quads(true) --> !,		% results from empty triple pattern
	[].
query_quads(A) -->
	{ quad(A) },
	[A].

quad(rdf(_,_,_)).
quad(rdf(_,_,_,_)).

resolve_graph_or_special(graph(Graph0), graph(Graph), State) :- !,
	resolve_iri(Graph0, Graph, State).
resolve_graph_or_special(Special, Special, _).

resolve_with(without, default, _).
resolve_with(with(IRI0), graph(IRI), State) :-
	resolve_iri(IRI0, IRI, State).


		 /*******************************
		 *	   STEAD FASTNESS	*
		 *******************************/

%%	steadfast(Q0, Q) is det.
%
%	Make Q0 steadfast. The problem  is   that  the  SPARQL semantics
%	assume bottom-up evaluation. Top-down evaluation yields the same
%	result as long as the  code   is  steadfast. Unfortunately, some
%	queries are not. This applies   notably to expression evaluation
%	in BIND. We fix this by   rewriting copying non-stead-fast parts
%	of the query and a post-execution unification.

steadfast(Q0, sparql_group(Q1, AT0, AT1)) :-
	phrase(non_steadfast(Q0), NonSteadFast),
	NonSteadFast \== [], !,
	term_variables(Q0, AllVars),
	sort(AllVars, AllSorted),
	sort(NonSteadFast, NSFSorted),
	ord_subtract(AllSorted, NSFSorted, SteadFast),
	STF =.. [v|SteadFast],
	copy_term(STF-Q0, STF1-Q1),
	STF = STF1,
	unifiable(Q0, Q1, Unifier),
	maplist(split_assignment, Unifier, A0, A1),
	AT0 =.. [v|A0],
	AT1 =.. [v|A1].
steadfast(Q0, sparql_group(Q0)).


split_assignment(A=B, A, B).

non_steadfast(Var) -->
	{ var(Var) }, !.
non_steadfast((A,B)) --> !,
	non_steadfast(A),
	non_steadfast(B).
non_steadfast((A;B)) --> !,
	non_steadfast(A),
	non_steadfast(B).
non_steadfast((A->B)) --> !,
	non_steadfast(A),
	non_steadfast(B).
non_steadfast((A*->B)) --> !,
	non_steadfast(A),
	non_steadfast(B).
non_steadfast(\+A) --> !,
	non_steadfast(A).
non_steadfast(sparql_eval(Expr, _Var)) --> !,
	term_variables(Expr).
non_steadfast(sparql_true(Expr)) --> !,
	term_variables(Expr).
non_steadfast(_) -->
	[].


		 /*******************************
		 *	COMPILE EXPRESSIONS	*
		 *******************************/

%%	compile_expression(+Expression, -Var, -Goal, +State0, -State)
%
%	Compile an expression into a (compound)   goal that evaluates to
%	the variable var. This version is  not realy compiling. Its just
%	the entry point for a future compiler.

compile_expression(bind(Expr,var(VarName)), Var, Goal, State0, State) :- !,
	resolve_var(VarName, Var, State0, State1),
	compile_expression(Expr, Var, Goal, State1, State).
compile_expression(Expr0, Var, Goal, State0, State) :-
	resolve_expression(Expr0, Expr, Q, State0, State),
	(   primitive(Expr)
	->  Var = Expr,
	    Goal = Q
	;   mkconj(Q, sparql_eval(Expr, Var), Goal)
	).

primitive(Var)  :- var(Var), !.
primitive(Atom) :- atom(Atom).		% IRI, '$null$'


		 /*******************************
		 *	    SPARQL DCG		*
		 *******************************/

:- discontiguous term_expansion/2.

:- if(current_predicate(string_codes/2)).
goal_expansion(keyword(S,L,T), keyword(Codes,L,T)) :-
	string(S),
	string_codes(S, Codes).
goal_expansion(must_see_keyword(S,L,T), must_see_keyword(Codes,L,T)) :-
	string(S),
	string_codes(S, Codes).
:- endif.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
From A.7. We keep the same naming and   order of the productions to make
it as easy as possible to verify the correctness of the parser.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	query(-Prologue, -Query)//

sparql_query(Prologue, Query, In, Out) :-
	catch(query(Prologue, Query, In, Out),
	      E,
	      add_error_location(E, In)).

query(Prologue, Query) -->		% [2]
	skip_ws,
	prologue(Prologue),
	(   select_query(Query)
	;   construct_query(Query)
	;   describe_query(Query)
	;   ask_query(Query)
	;   update_query(Query)
	), !.

%%	prologue(-Decls)//
%
%	The  Prologue  consists  of  zero  or    more  BASE  and  PREFIX
%	declarations. The result is the last   BASE declaration and each
%	PREFIX is resolved against the last preceeding BASE declaration.

prologue(Prologue) -->	% [4]
	prologue_decls(0, Base, Decls),
	{   Base == 0
        ->  Prologue = prologue(Decls)
	;   Prologue = prologue(Base, Decls)
	}.

prologue_decls(_, Base, Decls) -->
	base_decl(Base1), !,
	prologue_decls(Base1, Base, Decls).
prologue_decls(Base0, Base, [H|T]) -->
	prefix_decl(H, Base0), !,
	prologue_decls(Base0, Base, T).
prologue_decls(Base, Base, []) -->
	"".

%%	base_decl(-Base:uri)// is semidet.
%
%	Match "base <URI>".

base_decl(Base) -->			% [5]
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
%	select(Projection, DataSets, Query, Solutions)

select_query(select(Projection, DataSets, Query, Solutions)) --> % [7]
	select_clause(Projection, Solutions, S0),
	data_set_clauses(DataSets),
	where_clause(QWhere),
	solution_modifier(S0),
	values_clause(QValue),
	{ mkconj(QWhere, QValue, Query) }.

%%	sub_select(-SubSelect)//

sub_select(sub_select(Projection, Query, Solutions)) --> % [8]
	select_clause(Projection, Solutions, S0),
	where_clause(WQuery),
	solution_modifier(S0),
	values_clause(QValues),
	{ mkconj(WQuery, QValues, Query) }.


select_clause(Projection, Solutions, S0) --> % [9]
	keyword("select"),
	(   keyword("distinct")
	->  { Solutions = distinct(S0) }
	;   keyword("reduced")
	->  { Solutions = reduced(S0) }
	;   { Solutions = S0 }
	),
	select_projection(Projection).

%%	select_projection(-Projection)// is det.
%
%	Process the projection of a select query.  Projection is one of
%
%	  - *
%	  - List of variables
%	  - projection(ListOfVars, Binding)
%
%	  Where Binding is a conjunction of bind(Expression, Var)

select_projection(*) --> "*", !, skip_ws.
select_projection(projection([H|T], B)) -->
	projection_elt(H, true, B1),
	projection_elts(T, B1, B), !.
select_projection(_) -->
	syntax_error(projection_expected).

projection_elts([H|T], B0, B) -->
	projection_elt(H, B0, B1),
	projection_elts(T, B1, B).
projection_elts([], B, B) -->
	[].

projection_elt(Var, B, B) -->
	var(Var), !.
projection_elt(Var, B0, B) -->
	"(", skip_ws,
	(   expression(Expr), must_see_keyword("as"), var(Var),
	    must_see_close_bracket
	->  skip_ws,
	    { mkconj(B0, bind(Expr, Var), B) }
	;   syntax_error(illegal_projection)
	).

%%	construct_query(-Construct)// is semidet.
%
%	Processes "construct ..." into a term
%
%	construct(Template, DataSets, Query, Solutions)

construct_query(construct(Template, DataSets, Query, Solutions)) --> % [10]
	keyword("construct"),
	(   construct_template(Template),
	    data_set_clauses(DataSets),
	    where_clause(QWhere),
	    solution_modifier(Solutions)
	;   data_set_clauses(DataSets),
	    keyword("where"),
	    (	"{", skip_ws,
		triples_template(Template, []),
		"}"
	    ->	skip_ws,
		{QWhere = Template}
	    ;	syntax_error(triples_template_expected)
	    ),
	    solution_modifier(Solutions)
	),
	values_clause(QValue),
	{ mkconj(QWhere, QValue, Query) }.

%%	describe_query(-Describe)// is semidet.
%
%	Processes "describe ..." into a term
%
%	describe(Projection, DataSets, Query, Solutions)

describe_query(describe(Projection, DataSets, Query, Solutions)) --> % [11]
	keyword("describe"),
	desc_projection(Projection),
	data_set_clauses(DataSets),
	(where_clause(QWhere) -> [] ; {QWhere = true}),
	solution_modifier(Solutions),
	values_clause(QValue),
	{ mkconj(QWhere, QValue, Query) }.

desc_projection(*) --> "*", !, skip_ws.
desc_projection(projection([H|T], true)) -->
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
%

ask_query(ask(DataSets, Query, Solutions)) --> % [12]
	keyword("ask"),
	data_set_clauses(DataSets),
	where_clause(QWhere),
	solution_modifier(Solutions),
	values_clause(QValue),
	{ mkconj(QWhere, QValue, Query) }.

data_set_clauses([H|T]) -->		% [13*]
	dataset_clause(H), !,
	data_set_clauses(T).
data_set_clauses([]) -->
	[].

%%	dataset_clause(-Src)//

dataset_clause(Src) -->			% [13]
	keyword("from"),
	(   default_graph_clause(Src)
	->  []
	;   named_graph_clause(Src)
	).

%%	default_graph_clause(-Src)

default_graph_clause(Src) -->		% [14]
	source_selector(Src).

%%	named_graph_clause(Graph)//

named_graph_clause(Src) -->		% [15]
	keyword("named"),
	source_selector(Src).

%%	source_selector(-Src)//

source_selector(Src) -->		% [16]
	iri_ref(Src).

%%	where_clause(-Pattern)//

where_clause(Pattern) -->		% [17]
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
%	    solutions(Group, Having, Order, Limit, Offset)
%
%	Where
%
%	    * Group
%	    * Having
%	    * Order
%	    * Limit
%	    * Offset

solution_modifier(Modifier) -->		% [18]
	{ Modifier = solutions(Group, Having, Order, Limit, Offset) },
	( group_clause(Group)   -> [] ; { Group  = [] } ),
	( having_clause(Having) -> [] ; { Having = true } ),
	( order_clause(Order)   -> [] ; { Order  = unsorted } ),
	limit_offset_clauses(Limit, Offset).

limit_offset_clauses(Limit, Offset) -->
	limit_clause(Limit), !,
	( offset_clause(Offset) -> [] ; { Offset = 0 } ).
limit_offset_clauses(Limit, Offset) -->
	offset_clause(Offset), !,
	( limit_clause(Limit)   -> [] ; { Limit  = inf } ).
limit_offset_clauses(inf, 0) --> [].

%%	group_clause(-Group)// is semidet.

group_clause([G0|Groups]) -->
	keyword("group"),
	must_see_keyword("by"),
	must_see_group_condition(G0),
	group_conditions(Groups).

group_conditions([Group|T]) -->
	group_condition(Group), !,
	group_conditions(T).
group_conditions([]) -->
	"".

must_see_group_condition(G) -->
	group_condition(G), !.
must_see_group_condition(_) -->
	syntax_error(group_condition_expected).

group_condition(Exp) -->
	built_in_call(Exp), !.
group_condition(Exp) -->
	function_call(Exp), !.
group_condition(Exp) -->
	as_expression(Exp), !.
group_condition(Exp) -->
	var(Exp), !.

%%	as_expression(-Exp)// is det.
%
%	Processes '(' Expression ( 'AS' Var )? ')' into one of
%
%	    * bind(Expression, Var)
%	    * Expression

as_expression(Exp) -->
	"(", skip_ws, must_see_expression(E),
	(   keyword("as")
	->  must_see_var(Var),
	    {Exp = bind(E, Var)}
	;   {Exp = E}
	), ")", skip_ws.


%%	having_clause(-Having)// is semidet.

having_clause(ebv(C)) -->
	keyword("having"),
	must_see_having_condition(C0),
	having_conditions(C1),
	{ mkand(C0, C1, C) }.

having_conditions(C) -->
	having_condition(C0), !,
	having_conditions(C1),
	{ mkand(C0, C1, C) }.
having_conditions(true) -->
	"".

mkand(true, X, X).
mkand(X, true, X).
mkand(X, Y, and(X,Y)).


must_see_having_condition(C) -->
	having_condition(C), !.
must_see_having_condition(_) -->
	syntax_error(having_condition_expected).

having_condition(C) -->
	constraint(C).


%%	order_clause(-Order)//

order_clause(order_by([H|T])) -->
	keyword("order"), must_see_keyword("by"),
	must_be_order_condition(H),
	order_conditions(T).

order_conditions([H|T]) -->
	order_condition(H), !,
	order_conditions(T).
order_conditions([]) -->
	[].

must_be_order_condition(Cond) -->
	order_condition(Cond), !.
must_be_order_condition(_) -->
	syntax_error(order_condition_expected).

%%	order_condition(-Order)//

order_condition(ascending(Expr)) -->
	keyword("asc"), !,
	bracketted_expression(Expr).
order_condition(descending(Expr)) -->
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


%%	values_clause(-Query)// is det.
%
%	Query is one of
%
%	  * var_in(Var, Values)
%	  * vars_in(ListOfVar, ListOfValues)
%	  * true

values_clause(Q) -->			% [28]
	keyword("values"), !,
	data_block(Q).
values_clause(true) -->
	"".

%%	update_query(-UpdatedInfo)// is semidet.
%
%	True when input is a valid SPARQL update request.

update_query(update(Updates)) -->
	update(Updates).

update(Updates) -->
	(   update1(U1)
	->  { Updates = [U1|Update] },
	    (  ";"
	    ->	skip_ws,
		must_see_update(Update)
	    ;	{ Update = [] }
	    )
	;   { Updates = [] }
	).

must_see_update(Update) -->
	update(Update), !.
must_see_update(_) -->
	syntax_error(update_expected).

update1(Update) -->
	get_keyword(Action),
	update1(Action, Update), !.
update1(Update) -->
	modify(Update).

%%	update1(+Keyword, -UpdatedAction)// is semidet.

update1(load, load(Verbose, IRI, Graph)) -->
	silent(Verbose),
	iri_ref(IRI),
	(   keyword("into")
	->  graph_ref(GraphIRI),
	    {Graph = graph(GraphIRI)}
	;   {Graph = default}
	).
update1(clear, clear(Verbose, GraphRefAll)) -->
	silent(Verbose),
	graph_ref_all(GraphRefAll).
update1(drop, drop(Verbose, GraphRefAll)) -->
	silent(Verbose),
	graph_ref_all(GraphRefAll).
update1(create, create(Verbose, GraphRef)) -->
	silent(Verbose),
	graph_ref(GraphRef).
update1(add, add(Verbose, GraphOrDefaultFrom, GraphOrDefaultTo)) -->
	silent(Verbose),
	graph_or_default(GraphOrDefaultFrom),
	must_see_keyword("to"),
	graph_or_default(GraphOrDefaultTo).
update1(move, move(Verbose, GraphOrDefaultFrom, GraphOrDefaultTo)) -->
	silent(Verbose),
	graph_or_default(GraphOrDefaultFrom),
	must_see_keyword("to"),
	graph_or_default(GraphOrDefaultTo).
update1(copy, copy(Verbose, GraphOrDefaultFrom, GraphOrDefaultTo)) -->
	silent(Verbose),
	graph_or_default(GraphOrDefaultFrom),
	must_see_keyword("to"),
	graph_or_default(GraphOrDefaultTo).
update1(insert, insert_data(Quads)) -->
	keyword("data"), !,
	quad_data(Quads).
update1(delete, delete_data(Quads)) -->
	keyword("data"), !,
	quad_data(Quads).
update1(delete, delete_where(Quads)) -->
	keyword("where"), !,
	quad_pattern(Quads).

%%	modify(-Updated)//

modify(modify(WithIRI, InsDel, Using, Pattern)) --> % [41]
	optional_with(WithIRI),
	(   delete_clause(Del),
	    (	insert_clause(Ins)
	    ->	{ InsDel = replace(Del,Ins) }
	    ;	{ InsDel = delete(Del) }
	    )
	->  ""
	;   insert_clause(Ins),
	    { InsDel = insert(Ins) }
	),
	using_clauses(Using),
	must_see_keyword("where"),
	must_see_group_graph_pattern(Pattern).

optional_with(with(IRI)) -->
	keyword("with"), !,
	must_see_iri(IRI).
optional_with(without) -->
	"".

delete_clause(Quads) -->
	keyword("delete"),
	quad_pattern(Quads).
insert_clause(Quads) -->
	keyword("insert"),
	quad_pattern(Quads).

silent(silent) -->
	keyword("silent"), !.
silent(error) -->
	"".

using_clauses([U0|T]) -->
	keyword("using"), !,
	(   keyword("named"),
	    must_see_iri(IRI)
	->  { U0 = named(IRI) }
	;   must_see_iri(U0)
	),
	using_clauses(T).
using_clauses([]) -->
	"".

graph_ref(Graph) -->
	keyword("graph"),
	must_see_iri(Graph).

graph_ref_all(graph(Graph)) -->
	graph_ref(Graph), !.
graph_ref_all(default) -->
	keyword("default").
graph_ref_all(named) -->
	keyword("named").
graph_ref_all(all) -->
	keyword("all").

graph_or_default(default) -->
	keyword("default"), !.
graph_or_default(graph(Graph)) -->
	(   keyword("graph")
	->  ""
	;   ""
	),
	must_see_iri(Graph).

quad_pattern(Quads) -->				% [48]
	quad_data(Quads).

quad_data(Quads) -->
	"{", skip_ws,
	(   quads(Quads),
	    "}"
	->  skip_ws
	;   syntax_error(quads_expected)
	).

%%	quads(-Quads)//
%
%	Quads is a list of triples and graph(Graph,Triples)

quads(Quads) -->
	triples_template(Quads, Tail), !,
	quads_conts(Tail, []).
quads(Quads) -->
	quads_conts(Quads, []).

quads_conts(Quads, Tail) -->
	quads_cont(Quads, Tail2), !,
	quads_conts(Tail2, Tail).
quads_conts(Quads, Quads) -->
	"".

quads_cont([Graph|Tail0], Tail) -->
	quads_not_triples(Graph),
	optional_dot,
	(   triples_template(Tail0, Tail)
	->  ""
	;   {Tail0=Tail}
	).

quads_not_triples(graph(IRI, Triples)) -->
	keyword("graph"),
	var_or_iri_ref(IRI),
	must_see_open_brace,
	(   triples_template(Triples, [])
	->  ""
	;   {Triples=[]}
	),
	must_see_close_brace.


%%	data_block(-DataBlock)// is det.
%
%	DataBlock is one of
%
%	  * var_in(Var, ListOfValues)
%	  * vars_in(Vars, ListOfValues)

data_block(Values) -->
	inline_data_one_var(Values), !.
data_block(Values) -->
	inline_data_full(Values).

inline_data_one_var(var_in(Var, Values)) -->
	var(Var),
	inline_values(Values).

inline_values(Values) -->
	(   datablock_body(Values)
	->  ""
	;   datablock_body_full(ListValues)
	->  { maplist(single_body, ListValues, Values) }
	;   syntax_error(datablock_values_expected)
	).

single_body([Var], Var).

datablock_body(Values) -->
	"{", skip_ws, datablock_values(Values), "}", skip_ws.

datablock_values([V0|T]) -->
	datablock_value(V0), !,
	datablock_values(T).
datablock_values([]) -->
	"".

datablock_value(V) -->
	iri_ref(V), !.
datablock_value(V) -->
	rdf_literal(V), !.
datablock_value(V) -->
	numeric_literal(V), !.
datablock_value(B) -->
	boolean_literal(B), !.
datablock_value(_) -->			% UNDEF acts as a variable
	keyword("undef").

inline_data_full(InlineData) -->
	"(", skip_ws, vars(Vars),
	(   ")"
	->  skip_ws
	;   syntax_error(expected(')'))
	),
	(   { Vars = [Var] }
	->  inline_values(Values),
	    { InlineData = var_in(Var, Values) }
	;   datablock_body_full(Values)
	->  { InlineData = vars_in(Vars, Values) }
	;   syntax_error(datablock_values_expected)
	), !.

datablock_body_full(Values) -->
	"{", skip_ws,
	(   datablock_values_full(Values), "}"
	->  skip_ws
	;   syntax_error(datablock_values_expected)
	).

datablock_values_full([V0|T]) -->
	datablock_value_full(V0), !,
	datablock_values_full(T).
datablock_values_full([]) -->
	"".

datablock_value_full(List) -->
	"(", skip_ws,
	datablock_values(List),
	must_see_close_bracket.

vars([H|T]) -->
	var(H), !,
	vars(T).
vars([]) --> "".


%%	minus_graph_pattern(-Pattern) is det.

minus_graph_pattern(Pattern) -->
	keyword("minus"),
	must_see_group_graph_pattern(Pattern).

%%	triples_template(-Triples, Tail)//

triples_template(Triples, Tail) -->	% [52]
	triples_same_subject(Triples, Tail0),
	(   "."
	->  skip_ws,
	    (	triples_template(Tail0, Tail)
	    ->	""
	    ;	{Tail = Tail0}
	    )
	;   {Tail = Tail0}
	).


%%	group_graph_pattern(P)//
%

group_graph_pattern(group(P)) -->		% [53]
	"{", skip_ws,
	(   sub_select(P0)
	;   group_graph_pattern_sub(P0)
	;   syntax_error(graph_pattern_expected)
	), !,
	(   "}"
	->  skip_ws,
	    { resolve_bnodes(P0, P) }
	;   syntax_error(expected('}'))
	).


%%	group_graph_pattern_sub(P)//

group_graph_pattern_sub(P) -->		% [54]
	triples_block(P0, []), !,
	group_graph_pattern_sub_cont(P0, P).
group_graph_pattern_sub(P) -->
	group_graph_pattern_sub_cont(true, P).

%%	group_graph_pattern_sub_cont(+PLeft, P)//
%
%	Matches ( GraphPatternNotTriples '.'? TriplesBlock? )*

group_graph_pattern_sub_cont(PLeft, P) -->
	group_graph_pattern_sub_cont_1(PLeft, P0), !,
	group_graph_pattern_sub_cont(P0, P).
group_graph_pattern_sub_cont(PLeft, PLeft) --> "".

group_graph_pattern_sub_cont_1(PLeft, P) -->
	minus_graph_pattern(PRight), !,
	{ P = minus(PLeft, PRight) }.
group_graph_pattern_sub_cont_1(PLeft, P) -->
	graph_pattern_not_triples(P0),
	(   "."
	->  skip_ws
	;   ""
	),
	(   triples_block(P1, [])
	->  { mkconj(P0, P1, P2),
	      mkconj(PLeft, P2, P)
	    }
	;   { mkconj(PLeft, P0, P) }
	).


%%	triples_block(-Triples, ?Tail)//

triples_block(Triples, Tail) -->	% [55]
	triples_same_subject_path(Triples, Tail0),
	(   "."
	->  skip_ws,
	    (	triples_block(Tail0, Tail)
	    ->	""
	    ;	{ Tail = Tail0 }
	    )
	;   { Tail = Tail0 }
	).


one_dot -->
	".", !, skip_ws,
	(   "."
	->  syntax_error(double_dot)
	;   ""
	).

optional_dot --> ".", skip_ws.
optional_dot --> "".


%%	graph_pattern_not_triples(-Pattern)//

graph_pattern_not_triples(P) --> group_or_union_graph_pattern(P), !.
graph_pattern_not_triples(P) --> optional_graph_pattern(P), !.
graph_pattern_not_triples(P) --> graph_graph_pattern(P), !.
graph_pattern_not_triples(P) --> service_graph_pattern(P), !.
graph_pattern_not_triples(P) --> filter(P).
graph_pattern_not_triples(P) --> bind(P).
graph_pattern_not_triples(P) --> inline_data(P).

%%	optional_graph_pattern(Pattern)//

optional_graph_pattern(Pattern) -->	% [57]
	keyword("optional"),
	must_see_group_graph_pattern(P0),
	{ Pattern = optional(P0) }.

%%	graph_graph_pattern(-Graph)// is semidet
%
%	Processes a "graph ..." clause into
%
%	graph(Graph, Pattern)

graph_graph_pattern(graph(Graph, Pattern)) --> % [58]
	keyword("graph"), !,
	var_or_iri_ref(Graph),
	must_see_group_graph_pattern(Pattern).

%%	service_graph_pattern(P)//

service_graph_pattern(service(Silent, VarOrIRI, GroupGraphPattern)) --> % [59]
	keyword("service"), !,
	silent(Silent),
	var_or_iri_ref(VarOrIRI),
	group_graph_pattern(GroupGraphPattern).

%%	bind(P)

bind(bind(Expr, Var)) -->		% [60]
	keyword("bind"), !,
	must_see_open_bracket,
	must_see_expression(Expr),
	must_see_keyword("as"),
	must_see_var(Var),
	must_see_close_bracket.

%%	inline_data(Data)

inline_data(Values) -->
	keyword("values"),
	data_block(Values).


%%	group_or_union_graph_pattern(-Pattern)//

group_or_union_graph_pattern(Pattern) --> % [67]
	group_graph_pattern(P0),
	add_union(P0, Pattern).

add_union(P0, (P0;P)) -->
	keyword("union"), !,
	must_see_group_graph_pattern(P1),
	add_union(P1, P).
add_union(P, P) -->
	[].


%%	filter(-Filter)//

filter(filter(Exp)) -->
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
%

arg_list(ArgList) -->			% [71]
	"(", skip_ws,
	optional_distinct(ArgList, List),
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

%%	optional_distinct(-WrappedValue, -RealValue)//
%
%	Wrap argument in distinct(PlainArg)  if   there  is a =distinct=
%	keyword.

optional_distinct(E, E1) -->
	keyword("distinct"), !,
	{ E = distinct(E1) }.
optional_distinct(E, E) --> "".


arg_list_cont([H|T]) -->
	",", !, skip_ws,
	must_see_expression(H),
	arg_list_cont(T).
arg_list_cont([]) -->
	[].

%%	expression_list(-Expressions)//

expression_list(ExprList) -->
	"(", skip_ws,
	(   expression(A0)
	->  arg_list_cont(As),
	    {ExprList = [A0|As]}
	;   {ExprList = []}
	),
	(   ")"
	->  []
	;   syntax_error(expression_expected)
	),
	skip_ws.

%%	construct_template(Triples)// is semidet.

construct_template(Triples) -->
	"{", skip_ws,
	(   construct_triples(Triples), "}"
	->  skip_ws
	;   syntax_error(construct_template_expected)
	).

%%	construct_triples(-List)//

construct_triples(List) -->
	construct_triples(List, []).

construct_triples(List, T) -->
	triples_same_subject(List, T0), !,
	(   one_dot
	->  (   peek(0'})
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

%%	property_list(-Properties, -Triples, ?TriplesTail)//

property_list(L, Triples, Tail) -->
	property_list_not_empty(L, Triples, Tail), !.
property_list([], Tail, Tail) --> [].

%%	property_list_not_empty(-Properties, -Triples, ?TriplesTail)//
%

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

object_list(List, Triples, Tail) -->	% [79]
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

object(Obj, Triples, Tail) -->		% [80]
	graph_node(Obj, Triples, Tail).

%%	verb(-E)//

verb(E) --> var_or_iri_ref(E), !.	% [78]
verb(E) --> "a", skip_ws, { rdf_equal(E, rdf:type) }.


		 /*******************************
		 *	      PATHS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A property path basically describes a   complex relation from a resource
to another resource. We represent a path   as rdf(S,P,O), where P is one
of



See http://www.w3.org/TR/sparql11-query/#propertypaths
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


%%	triples_same_subject_path(-Triples, ?Tail)//
%
%	Similar to triples_same_subject//2, but   the resulting property
%	of each triple can be a path expression.

triples_same_subject_path(Triples, Tail) -->
	var_or_term(Subject), !,
	property_list_path_not_empty(Props, Triples, Tail0),
	{ make_triples_same_subject(Props, Subject, Tail0, Tail) }.
triples_same_subject_path(Triples, Tail) -->
	triples_node_path(Subject, Triples, Tail0),
	property_list_path(Props, Tail0, Tail1),
	{ make_triples_same_subject(Props, Subject, Tail1, Tail) }.

property_list_path(Props, Triples, Tail) -->
	property_list_path_not_empty(Props, Triples, Tail), !.
property_list_path([], Triples, Triples) -->
	"".

property_list_path_not_empty(Props, Triples, Tail) --> % [83]
	verb_path_or_simple(Path),
	must_see_object_list_path(OL, Triples, Tail0),
	{ mk_proplist(OL, Path, Props, T) },
	(   ";", skip_ws
	->  verb_object_lists(T, Tail0, Tail)
	;   { T = [],
	      Tail = Tail0
	    }
	).

%%	verb_object_lists(-Properties, -Triples, ?Tail)// is det.
%
%	Parses ( ';' ( ( VerbPath | VerbSimple ) ObjectList )? )*

verb_object_lists(Props, Triples, Tail) -->
	verb_path_or_simple(Path), !,
	must_see_object_list(OL, Triples, Tail0),
	{ mk_proplist(OL, Path, Props, T) },
	(   ";", skip_ws
	->  verb_object_lists(T, Tail0, Tail)
	;   { T = [],
	      Tail = Tail0
	    }
	).
verb_object_lists([], Triples, Triples) --> "".


verb_path_or_simple(Path) -->
	verb_path(Path), !.
verb_path_or_simple(Path) -->
	verb_simple(Path).

verb_path(Path) -->			% [84]
	path(Path).

verb_simple(Var) -->
	var(Var).

must_see_object_list_path(Objects, Triples, Tail) -->
	object_list_path(Objects, Triples, Tail), !.
must_see_object_list_path(_,_,_) -->
	syntax_error(object_list_path_expected).

object_list_path(Objects, Triples, Tail) -->
	object_path(H, Triples, Tail0),
	(   ",", skip_ws
	->  { Objects = [H|T] },
	    object_list_path(T, Tail0, Tail)
	;   { Objects = [H],
	      Tail = Tail0
	    }
	).

object_path(Object, Triples, Tail) -->
	graph_node_path(Object, Triples, Tail).

path(Path) -->
	path_alternative(Path).

path_alternative(PathAlt) -->
	path_sequence(S0),
	(   "|"
	->  skip_ws,
	    {PathAlt = (S0;S1)},
	    path_alternative(S1)
	;   {PathAlt = S0}
	).

path_sequence(PSeq) -->
	path_elt_or_inverse(S0),
	(   "/"
	->  skip_ws,
	    {PSeq = S0/PSeq2},
	    path_sequence(PSeq2)
	;   {PSeq = S0}
	).

path_elt_or_inverse(^(PathElt)) -->
	"^", !, skip_ws,
	path_elt(PathElt).
path_elt_or_inverse(PathElt) -->
	path_elt(PathElt).

%%	path_elt(PathElt)
%
%	One of [?*+=](PathPrimary)

path_elt(PathElt) -->
	path_primary(PP),
	path_mod(PP, PathElt).

path_mod(PP, ?(PP)) --> "?", \+ varname(_), !, skip_ws.
path_mod(PP, *(PP)) --> "*", !, skip_ws.
path_mod(PP, +(PP)) --> "+", !, skip_ws.
path_mod(PP, PP) --> "".


%%	path_primary(-PathPrimary)//

path_primary(IRI) -->			% [94]
	iri_ref_or_a(IRI), !.
path_primary(!(PathNegatedPropertySet)) -->
	"!", !, skip_ws,
	path_negated_property_set(PathNegatedPropertySet).
path_primary(Path) -->
	"(", !, skip_ws,
	(   path(Path), ")"
	->  skip_ws
	;   syntax_error(path_expected)
	).
path_primary(distinct(Path)) -->
	keyword("distinct"), !,
	(   "(", skip_ws, path(Path), ")"
	->  skip_ws
	;   syntax_error(path_expected)
	).

path_negated_property_set(PathNegatedPropertySet) -->
	"(", !, skip_ws,
	(   paths_in_property_set(PathNegatedPropertySet),
	    ")"
	->  skip_ws
	;   syntax_error(path_one_in_property_set_expected)
	).
path_negated_property_set(PathNegatedPropertySet) -->
	path_one_in_property_set(PathNegatedPropertySet), !.

paths_in_property_set(P) -->
	path_one_in_property_set(P1),
	(   "|"
	->  skip_ws,
	    paths_in_property_set(P2),
	    { P=(P1;P2) }
	;   { P=P1 }
	).

path_one_in_property_set(^(IRI)) -->
	"^", !, skip_ws,
	iri_ref_or_a(IRI).
path_one_in_property_set(IRI) -->
	iri_ref_or_a(IRI).

iri_ref_or_a(IRI) -->
	iri_ref(IRI).
iri_ref_or_a(RdfType) -->
	"a", !, skip_ws,
	{ rdf_equal(RdfType, rdf:type) }.


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

%%	triples_node_path(-Subj, -Triples, ?Tail)//

triples_node_path(Subj, Triples, Tail) -->
	collection_path(Subj, Triples, Tail), !.
triples_node_path(Subj, Triples, Tail) -->
	blank_node_property_list_path(Subj, Triples, Tail).

%%	blank_node_property_list_path(-Subj, -Triples, ?TriplesTail)//

blank_node_property_list_path(Subj, Triples, Tail) -->
	"[", skip_ws,
	property_list_path_not_empty(List, Triples, T0),
	"]", skip_ws,
	{ make_triples_same_subject(List, Subj, T0, Tail) }.

%%	collection(-Subj, -Triples, ?Tail)//

collection(collection([H|T]), Triples, Tail) -->
	"(", skip_ws,
	graph_node(H, Triples, T0),
	graph_nodes(T, T0, Tail),
	")", skip_ws.

%%	collection_path(-Subj, -Triples, ?Tail)//

collection_path(collection([H|T]), Triples, Tail) -->
	"(", skip_ws,
	(   graph_node_path(H, Triples, Tail0),
	    graph_nodes_path(T, Tail0, Tail),
	    ")"
	->  skip_ws
	;   syntax_error(graph_node_path_expected)
	).


graph_nodes([H|T], Triples, Tail) -->
	graph_node(H, Triples, T0), !,
	graph_nodes(T, T0, Tail).
graph_nodes([], T, T) --> [].

graph_nodes_path([H|T], Triples, Tail) -->
	graph_node_path(H, Triples, T0), !,
	graph_nodes_path(T, T0, Tail).
graph_nodes_path([], T, T) --> [].

%%	graph_node(E, -Triples, ?TriplesTail)//

graph_node(E, T, T)       --> var_or_term(E), !.
graph_node(E, Triples, T) --> triples_node(E, Triples, T).

%%	graph_node_path(Node, Triples, Tail)//

graph_node_path(E, T, T)          --> var_or_term(E), !.
graph_node_path(E, Triples, Tail) --> triples_node_path(E, Triples, Tail).

%%	var_or_term(-E)//

var_or_term(E) --> var(E), !.
var_or_term(E) --> graph_term(E).

%%	var_or_iri_ref(-E)//

var_or_iri_ref(E) --> var(E), !.
var_or_iri_ref(E) --> iri_ref(E), !.

%%	var(-Var)//

var(var(Name)) -->
	(   var1(Name)
	->  []
	;   var2(Name)
	),
	skip_ws.

must_see_var(Var) -->
	var(Var), !.
must_see_var(_) -->
	syntax_error(var_expected).

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
	;   keyword("in")
	->  expression_list(List),
	    { E = in(E0, List) }
	;   keyword("not"), keyword("in")
	->  expression_list(List),
	    { E = not_in(E0, List) }
	;   { E = E0 }
	).

relational_op(=) --> "=".
relational_op(\=) --> "!=".
relational_op(>=) --> ">=".
relational_op(>) --> ">".
relational_op(Op) -->
	"<", \+ (iri_codes(_), ">"),
	(   "="
	->  { Op = (=<) }
	;   { Op = (<) }
	).

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
unary_expression(E)      -->		 primary_expression(E).


%%	primary_expression(-E)//

primary_expression(E) --> bracketted_expression(E), !.
primary_expression(E) --> built_in_call(E), !.
primary_expression(E) --> iri_ref_or_function(E), !.
primary_expression(E) --> rdf_literal(E), !.
primary_expression(E) --> numeric_literal(E), !.
primary_expression(E) --> boolean_literal(E), !.
primary_expression(E) --> var(E), !.


%%	bracketted_expression(-E)//

bracketted_expression(E) -->
	"(", skip_ws, must_see_expression(E), ")", skip_ws.

%%	built_in_call(-Call)//

built_in_call(F) -->			% [121]
	get_keyword(KWD),
	built_in_call(KWD, F).

built_in_call(KWD, F) -->
	{ built_in_function(KWD, Types) },
	must_see_open_bracket,
	arg_list(Types, Args),
	must_see_close_bracket, !,
	{   Args == []
	->  F = built_in(KWD)
	;   F =.. [KWD|Args]
	}.
built_in_call(KWD, F) -->
	aggregate_call(KWD, F), !.
built_in_call(coalesce, coalesce(List)) --> !,
	expression_list(List).
built_in_call(concat, concat(List)) --> !,
	expression_list(List).
built_in_call(substr, Substr) --> !,
	substring_expression(Substr).
built_in_call(replace, Replace) --> !,
	str_replace_expression(Replace).
built_in_call(regex, Regex) --> !,
	regex_expression(Regex).
built_in_call(exists, F) --> !,
	exists_func(F).
built_in_call(not, F) -->
	not_exists_func(F).

built_in_function(str,		  [expression]).
built_in_function(lang,		  [expression]).
built_in_function(langmatches,	  [expression, expression]).
built_in_function(datatype,	  [expression]).
built_in_function(bound,	  [var]).
built_in_function(iri,		  [expression]).
built_in_function(uri,		  [expression]).
built_in_function(bnode,	  [expression]).
built_in_function(bnode,	  []).
built_in_function(rand,		  []).
built_in_function(abs,		  [expression]).
built_in_function(ceil,		  [expression]).
built_in_function(floor,	  [expression]).
built_in_function(round,	  [expression]).
built_in_function(strlen,	  [expression]).
built_in_function(ucase,	  [expression]).
built_in_function(lcase,	  [expression]).
built_in_function(encode_for_uri, [expression]).
built_in_function(contains,	  [expression, expression]).
built_in_function(strstarts,	  [expression, expression]).
built_in_function(strends,	  [expression, expression]).
built_in_function(strbefore,	  [expression, expression]).
built_in_function(strafter,	  [expression, expression]).
built_in_function(year,		  [expression]).
built_in_function(month,	  [expression]).
built_in_function(day,		  [expression]).
built_in_function(hours,	  [expression]).
built_in_function(minutes,	  [expression]).
built_in_function(seconds,	  [expression]).
built_in_function(timezone,	  [expression]).
built_in_function(tz,		  [expression]).
built_in_function(now,		  []).
built_in_function(uuid,		  []).
built_in_function(struuid,	  []).
built_in_function(md5,		  [expression]).
built_in_function(sha1,		  [expression]).
built_in_function(sha256,	  [expression]).
built_in_function(sha384,	  [expression]).
built_in_function(sha512,	  [expression]).
built_in_function(coalesce,	  [expression_list]).
built_in_function(if,		  [expression, expression, expression]).
built_in_function(strlang,	  [expression, expression]).
built_in_function(strdt,	  [expression, expression]).
built_in_function(sameterm,	  [expression, expression]).
built_in_function(isiri,	  [expression]).
built_in_function(isuri,	  [expression]).
built_in_function(isblank,	  [expression]).
built_in_function(isliteral,	  [expression]).
built_in_function(isnumeric,	  [expression]).

term_expansion(built_in_function(f), Clauses) :-
	findall(built_in_function(F),
		( built_in_function(Name, Args),
		  length(Args, Argc),
		  functor(F, Name, Argc)
		),
		Clauses).

%%	built_in_function(?Term) is nondet.
%
%	Fact  that  describes  defined  builtin    functions.   Used  by
%	resolve_expression/4.

built_in_function(regex(_,_,_)).
built_in_function(replace(_,_,_,_)).
built_in_function(substr(_,_,_)).
built_in_function(substr(_,_)).
built_in_function(f).


arg_list([], []) --> "".
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
	must_see_open_bracket,
	must_see_expression(Target),
	must_see_comma,
	must_see_expression(Pattern),
	(   ",", skip_ws, must_see_expression(Flags)
	->  []
	;   {Flags = literal('')}
	),
	must_see_close_bracket.

%%	substring_expression(Expr)//

substring_expression(Expr) --> % [123]
	must_see_open_bracket,
	must_see_expression(Source),
	must_see_comma,
	must_see_expression(StartingLoc),
	(   ","
	->  skip_ws,
	    must_see_expression(Length),
	    { Expr = substr(Source, StartingLoc, Length) }
	;   { Expr = substr(Source, StartingLoc) }
	),
	must_see_close_bracket.

%%	must_see_comma// is det.
%%	must_see_open_bracket// is det.
%%	must_see_close_bracket// is det.
%%	must_see_punct(+C)// is det.
%
%	Demand  punctuation.  Throw  a  syntax  error  if  the  demanded
%	punctiation is not present.

must_see_comma         --> must_see_punct(0',).
must_see_open_bracket  --> must_see_punct(0'().
must_see_close_bracket --> must_see_punct(0')).
must_see_open_brace    --> must_see_punct(0'{).
must_see_close_brace   --> must_see_punct(0'}).

must_see_punct(C) -->
	[C], !, skip_ws.
must_see_punct(C) -->
	syntax_error(expected(C)).


%%	str_replace_expression(Expr)//

str_replace_expression(replace(Arg, Pattern, Replacement, Flags)) --> % [124]
	must_see_open_bracket,
	must_see_expression(Arg),
	must_see_comma,
	must_see_expression(Pattern),
	must_see_comma,
	must_see_expression(Replacement),
	(   ",", skip_ws, must_see_expression(Flags)
	    ->  []
	    ;   {Flags = literal('')}
	),
	must_see_close_bracket.

%%	exists_func(F)//

exists_func(exists(Pattern)) -->	% [125]
	must_see_group_graph_pattern(Pattern).

not_exists_func(not_exists(Pattern)) --> % [126]
	keyword("exists"),
	must_see_group_graph_pattern(Pattern).

%%	aggregate_call(+Keyword, -Aggregate)//
%
%	Renamed from =aggregate= to avoid confusion with popular predicate.

aggregate_call(count, Aggregate) -->		% [127]
	aggregate_count(Aggregate), !.
aggregate_call(Agg, Aggregate) -->
	{ aggregate_keyword(Agg) }, !,
	must_see_open_bracket,
	{ Aggregate =.. [Agg,AggArg] },
	optional_distinct(AggArg, AggExpr),
	expression(AggExpr),
	must_see_close_bracket.
aggregate_call(group_concat, Aggregate) -->
	aggregate_group_concat(Aggregate).

aggregate_keyword(sum).
aggregate_keyword(min).
aggregate_keyword(max).
aggregate_keyword(avg).
aggregate_keyword(sample).

aggregate_count(count(Count)) -->
	must_see_open_bracket,
	optional_distinct(Count, C1),
	(   "*"
	    ->  skip_ws,
	    { C1 = (*) }
	    ;   expression(C1)
	),
	must_see_close_bracket.


aggregate_group_concat(group_concat(Expr, literal(Sep))) -->
	must_see_open_bracket,
	optional_distinct(Expr, Expr2),
	expression(Expr2),
	(   ";"
	->  skip_ws,
	    must_see_keyword("separator"),
	    must_see_punct(0'=),
	    string(Sep)
	;   {Sep = ' '}			% default sep is a single space
	),
	must_see_close_bracket.

%%	aggregate_op(?Op) is nondet.
%
%	Declaration to support resolving aggregates

aggregate_op(count(_)).
aggregate_op(sum(_)).
aggregate_op(min(_)).
aggregate_op(max(_)).
aggregate_op(avg(_)).
aggregate_op(sample(_)).
aggregate_op(group_concat(_,_)).

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

must_see_iri(IRI) -->
	iri_ref(IRI), !.
must_see_iri(_) -->
	syntax_error(iri_expected).

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

iri_codes([H|T]) -->
	iri_code(H), !,
	iri_codes(T).
iri_codes([]) -->
	[].

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

%%	pn_chars_u(?Code)
%
%	Allows for _

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
	between(0'0, 0'9, Code), !.

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
	{ \+ last(Codes, 0'.) }, !.

ncchar_or_dots([H|T0], T) -->
	ncchar_or_dot(H),
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

pn_local(Atom) -->			% [169]
	localchar1(Codes, Tail),
	pn_local_suffix(Tail),
	{ atom_codes(Atom, Codes) }.

pn_local_suffix(Codes) -->
	pnchars(Codes, Tail),
	pnchar_last(Tail, []), !.
pn_local_suffix([]) -->
	"".

pnchars(List, Tail) -->
	pnchar(List, Tail0),
	pnchars(Tail0, Tail).
pnchars(T, T) --> "".

pnchar([C|T], T) -->
	[C],
	{ pnchar(C) }, !.
pnchar(Codes, Tail) -->
	plx(Codes, Tail).

pnchar(C) :- varchar(C).
pnchar(0'-).
pnchar(0'.).
pnchar(0':).

pnchar_last([C|T], T) -->
	[C],
	{ pnchar_last(C) }, !.
pnchar_last(Codes, Tail) -->
	plx(Codes, Tail).

pnchar_last(C) :- varchar(C).
pnchar_last(0':).


localchar1([Code|Tail], Tail) -->
	esc_code(Code),
	{ localchar1(Code) }, !.
localchar1(Codes, Tail) -->
	plx(Codes, Tail).

plx(Codes, Tail) -->
	percent(Codes, Tail).
plx(Codes, Tail) -->
	pn_local_esc(Codes, Tail).

percent(Codes, Tail) -->		% [171]
	"%", [H1,H2],
	{ code_type(H1, xdigit(_)),
	  code_type(H2, xdigit(_)),
	  Codes = [0'%,H1,H2|Tail]
	}.

localchar1(Code) :-
	pn_chars_u(Code), !.
localchar1(Code) :-
	between(0'0, 0'9, Code), !.
localchar1(0':).

pn_local_esc(List, T) -->		% [173]
	"\\",
	[C],
	{ pn_local_esc(C),
	  List = [C|T]
	}.

pnle('_~.-!$&\'()*+,;=/?#@%').

term_expansion(pn_local_esc(esc), Clauses) :-
	pnle(Atom),
	findall(pn_local_esc(C),
		( sub_atom(Atom, _, 1, _, Char),
		  char_code(Char, C)
		), Clauses).

pn_local_esc(esc).



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

%%	must_see_keyword(+Codes)

must_see_keyword(Codes) -->
	keyword(Codes), !.
must_see_keyword(Codes) -->
	{ atom_codes(Atom, Codes),
	  upcase_atom(Atom, Keyword)
	},
	syntax_error(expected(Keyword)).


%%	get_keyword(-Atom)
%
%	Get next identifier as lowercase

get_keyword(Atom) -->
	one_or_more_keyword_chars(Letters),
	{ atom_codes(Raw, Letters),
	  downcase_atom(Raw, Atom)
	},
	skip_ws.

one_or_more_keyword_chars([H|T]) -->
	keyword_char(H),
	keyword_chars(T).

keyword_chars([H|T]) -->
	keyword_char(H), !,
	keyword_chars(T).
keyword_chars([]) --> "".

keyword_char(C)   --> ascii_letter(C), !.
keyword_char(C)   --> digit(C), !.
keyword_char(0'_) --> "_".



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

peek(C, L, L) :-
	L = [C|_].
