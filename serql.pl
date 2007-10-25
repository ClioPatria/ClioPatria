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

:- module(serql,
	  [ serql_query/2,		% +Query, -Result
	    serql_query/3,		% +Query, -Result, +Options
	    serql_compile/3,		% +Query, -Compiled, +Options
	    serql_run/2			% +Compiled, -Reply
	  ]).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library('semweb/rdf_db')).
:- use_module(no_entailment, []).
:- use_module(rdfs_entailment, []).
:- use_module(library(debug)).
:- use_module(library(settings)).
:- use_module(rdf_optimise).
:- use_module(rdfql_util).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A Prolog path expression is a conjunction of rdf/3 statements. Parts may
be wrapped in opt/1 to indicate they are   optional  and nodes may be of
the form set(List) to indicate a conjunction of distinct values.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	serql_query(+Query, -Reply, +Module)
%	
%	Where Query is either a SeRQL  query   text  or  a parsed query.
%	Reply is, similar to the  ODBC  interface   a  term  of the form
%	row(Col1, Col2, ...) for SELECT statements or a term rdf(S,P,O).
%	The predicate is non-deterministic, returning   the  rows or RDF
%	statements one-by-one.


serql_query(Query, Result) :-
	serql_query(Query, Result,
		    [ entailment(rdf)
		    ]).

serql_query(Query, Result, Options) :-
	serql_compile(Query, Compiled, Options),
	serql_run(Compiled, Result).

%%	serql_compile(+Query, -Compiled, +Options)
%	
%	Compile a SeRQL query, returning the result in Compiled.  Options:
%	
%		* entailment(Entailment)
%		Entailment module to use.
%		
%		* type(-Type)
%		Return one of select(VarNames) or construct

serql_compile(Text, Compiled, Options) :-
	atom(Options), Options \== [], !,	% compatibility
	serql_compile(Text, Compiled, [entailment(Options)]).
serql_compile(Text, serql_query(Goal, ReplyTempl, Module), Options) :-
	serql_parse(Text, Query),
	compile(Query, Goal, ReplyTempl, Module, Options).

compile(select(Row0, VarNames, Path, Where, Distinct, Limit, Offset),
	select(Final, Solutions),
	Row,
	Module,
	Options) :-
	option(entailment(Entailment), Options, rdfs),
	entailment_module(Entailment, Module),
	mk_solutions(Distinct, Limit, Offset, Solutions),
	set_type(select(VarNames), Options),
	where_constraints(Where, Annotations),
	serql_compile_path(Path, select, Goal),
	remove_annotations(Annotations, where),
	projection_functions(Row0, Row, Select),
	(   setting(serql_parms:optimise_query, Def),
	    option(optimise(Opt), Options, Def),
	    Opt == true
	->  rdf_optimise((Goal,Where,Select), Optimised)
	;   Optimised = (Goal,Where,Select)
	),
	serql_select_bind_null(Optimised, Final),
	debug(serql(compiled), '~@',
	            [ portray_clause((q(Row) :- Final))
		    ]).
compile(construct(RPath, Path, Where, Distinct, Limit, Offset),
	construct(Final, Solutions),
	RDF,
	Module,
	Options) :-
	option(entailment(Entailment), Options, rdfs),
	entailment_module(Entailment, Module),
	mk_solutions(Distinct, Limit, Offset, Solutions),
	set_type(construct, Options),
	where_constraints(Where, Annotations),
	serql_compile_path(Path, construct, Goal),
	remove_annotations(Annotations, where),
	statements(RPath, Statements),
	entailment_module(Entailment, Module),
	(   setting(serql_parms:optimise_query, Def),
	    option(optimise(Opt), Options, Def),
	    Opt == true
	->  rdf_optimise((Goal,Where), Optimised)
	;   Optimised = (Goal,Where)
	),
	Final = (Optimised, serql_member_statement(RDF, Statements)),
	debug(serql(compiled), '~@',
	            [ portray_clause((q(RDF) :- Final))
		    ]).


%%	mk_solutions(+Distinct, +Limit, +Offset, -Term)
%	
%	Create a solutions-selecting term compatible to SPARQL.

mk_solutions(distinct, Limit, Offset,
	     distinct(solutions(unsorted, Limit, Offset))) :- !.
mk_solutions(_, Limit, Offset, solutions(unsorted, Limit, Offset)).

%%	set_type(+Type, +Options)
%	
%	Fill option type(X)

set_type(Type, Options) :-
	memberchk(type(T), Options), !,
	(   T = Type
	->  true
	;   functor(T, Expected, _),
	    functor(Type, Found, _),
	    throw(error(type_error(query_type(Expected), Found), _))
	).
set_type(_, _).

%%	serql_run(+Term, -Result)

serql_run(serql_query(Parsed, Reply, Module), Reply) :-
	serql_run(Parsed, Reply, Module).

serql_run(select(Goal, Solutions), Reply, Module) :-
	select_results(Solutions, Reply, Module:Goal).
serql_run(construct(Goal, Solutions), Reply, Module) :-
	select_results(Solutions, Reply, Module:Goal).

%%	select_results(+Spec, -Reply, :Goal)
%	
%	Apply ordering and limits on result-set.

select_results(distinct(solutions(Order, Limit, Offset)), Reply, Goal) :- !,
	select_results(distinct, Offset, Limit, Order, Reply, Goal).
select_results(solutions(Order, Limit, Offset), Reply, Goal) :-
	select_results(all, Offset, Limit, Order, Reply, Goal).


		 /*******************************
		 *	     COMPILER		*
		 *******************************/

%%	serql_compile_path(+PathExpr, +Type, -PrologGoal)
%	
%	Compile a Serql path expression into a plain Prolog goal.  Type
%	is one of 'select' or 'construct'.

serql_compile_path(rdf(S,P,O), Type, Conj) :-
	set(S, Set), !,
	make_set_subj_conj(Set, [], P, O, Type, Conj).
serql_compile_path(rdf(S,P,O), Type, Conj) :-
	set(O, Set), !,
	make_set_obj_conj(Set, [], S, P, Type, Conj).
serql_compile_path(rdf(S0, P, O), Type, Goal) :-
	reified(S0, S, GS), !,
	serql_compile_path(rdf(S, P, O), Type, G0),
	Goal = (G0, GS).
serql_compile_path(rdf(S, P, O0), Type, Goal) :-
	reified(O0, O, GS), !,
	serql_compile_path(rdf(S, P, O), Type, G0),
	Goal = (G0, GS).
serql_compile_path((A0,B0), Type, (A,B)) :- !,
	serql_compile_path(A0, Type, A),
	serql_compile_path(B0, Type, B).
serql_compile_path(optional(Id, A0), construct, (A *-> Id=true ; Id=false)) :- !,
	serql_compile_path(A0, construct, A).
serql_compile_path(optional(_, A0), select, (A *-> true ; true)) :- !,
	serql_compile_path(A0, select, A).
serql_compile_path(rdf(S,P,O0), _, Goal) :- !,
	resource_annotations(S, GS),
	resource_annotations(P, GP),
	object_annotations(O0, O, GO),
	clean_conj((GS, GP, rdf(S,P,O), GO), Goal).
serql_compile_path(G, _, G).

reified(0, _, _) :-			% catch variables
	!, fail.
reified(rdf(S,P,O), StatementId,
	(   rdf(StatementId, Type, Statement),
	    rdf(StatementId, Subject, S),
	    rdf(StatementId, Predicate, P),
	    rdf(StatementId, Object, O)
	)) :-
	rdf_equal(Type, rdf:type),
	rdf_equal(Subject, rdf:subject),
	rdf_equal(Predicate, rdf:predicate),
	rdf_equal(Object, rdf:object),
	rdf_equal(Statement, rdf:'Statement').



make_set_subj_conj([], _, _, _, _, true).	% should not happen
make_set_subj_conj([Last], [], P, O, Type, Goal) :- !,
	serql_compile_path(rdf(Last, P, O), Type, Goal).
make_set_subj_conj([Last], Diff, P, O, Type, (Goal, Diffs)) :- !,
	serql_compile_path(rdf(Last, P, O), Type, Goal),
	make_diff(Diff, Last, Diffs).
make_set_subj_conj([H|T], Diff, P, O, Type, (Goal, Diffs, More)) :- !,
	serql_compile_path(rdf(H, P, O), Type, Goal),
	make_diff(Diff, H, Diffs),
	make_set_subj_conj(T, [H|Diff], P, O, Type, More).


make_set_obj_conj([], _, _, _, _, true).	% should not happen
make_set_obj_conj([Last], [], S, P, Type, Goal) :- !,
	serql_compile_path(rdf(S, P, Last), Type, Goal).
make_set_obj_conj([Last], Diff, S, P, Type, (Goal, Diffs)) :- !,
	serql_compile_path(rdf(S, P, Last), Type, Goal),
	make_diff(Diff, Last, Diffs).
make_set_obj_conj([H|T], Diff, S, P, Type, (Goal, Diffs, More)) :- !,
	serql_compile_path(rdf(S, P, H), Type, Goal),
	make_diff(Diff, H, Diffs),
	make_set_obj_conj(T, [H|Diff], S, P, Type, More).


make_diff([], _, true).
make_diff([Last], To, (Last \== To)) :- !.
make_diff([H|T], To, (H \== To, More)) :-
	make_diff(T, To, More).
	

%%	statements(+Graph, -ListOfTriples)
%	
%	Extract  a  plain   list   of    triples   from   an   CONSTRUCT
%	path-expression. Optional parts of the   tree are represented as
%%	optional(Bool, ListOfTriples). Using CONSTRUCT *  (i.e. when the
%	executed path is the result  path)   the  goal  generated by the
%	compiler  will  unify  Bool  with  true    or  false.  See  also
%	member_statement/2.

statements(Graph, Statements) :-
	phrase(statements(Graph), Statements).

statements(rdf(S,P,O)) -->
	{ set(S, Set) }, !,
	subj_statements(Set, P, O).
statements(rdf(S,P0,O)) --> !,
	{   nonvar(P0),
	    map_builtin(P0, P)
	->  true
	;   P = P0
	},
	[ rdf(S,P,O) ].
statements((A,B)) --> !,
	statements(A),
	statements(B).
statements(optional(Id, A)) --> !,
	{ phrase(statements(A), OptionalStatements) },
	[ optional(Id, OptionalStatements) ].
statements(_) -->
	[].

term_expansion(map_builtin(B0, P0), map_builtin(B, P)) :-
	rdf_global_id(B0, B),
	rdf_global_id(P0, P).

map_builtin(serql:directSubClassOf,    rdfs:subClassOf).
map_builtin(serql:directSubPropertyOf, rdfs:subPropertyOf).
map_builtin(serql:directType,          rdf:type).


subj_statements([], _, _) -->
	[].
subj_statements([H|T], P, O) -->
	(   { set(O, Set) }
	->  obj_statements(Set, H, P)
	;   [ rdf(H, P, O) ]
	),
	subj_statements(T, P, O).

obj_statements([], _, _) -->
	[].
obj_statements([H|T], S, P) -->
	[ rdf(S, P, H) ],
	obj_statements(T, S, P).


set(Node, Set) :-
	nonvar(Node),
	Node = set(Set).


		 /*******************************
		 *	 SELECT FUNCTIONS	*
		 *******************************/

projection_functions(Row0, Row, Map) :-
	functor(Row0, Functor, Arity),
	functor(Row, Functor, Arity),
	projection_functions(0, Arity, Row0, Row, true, Map).

projection_functions(Arity, Arity, _, _, Map, Map) :- !.
projection_functions(I0, Arity, Row0, Row, Map0, Map) :-
	I is I0 + 1,
	arg(I, Row0, A0),
	(   var(A0)
	->  arg(I, Row, A0),
	    projection_functions(I, Arity, Row0, Row, Map0, Map)
	;   arg(I, Row, A),
	    add_conj(Map0, serql_eval(A0, A), Map1),
	    projection_functions(I, Arity, Row0, Row, Map1, Map)
	).
	    
add_conj(true, X, X) :- !.
add_conj(C0, G, (C0,G)).	    


		 /*******************************
		 *	WHERE CONSTRAINTS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The idea of this step  is  to   turn  where  clauses into constraints on
variables.

Supported annotations (in standard order of terms):

	any
	literal
	resource
	eq(Value)
	like(Pattern)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	where_constraints(+Goal, -Annotations)
%	
%	Each  annotation  is  either  a  plain   annotation  or  a  term
%%	or(ListOfAlternatives). The latter is used   if  different paths
%	through the control-structure yields different annotations.

where_constraints(Goal, Annotations) :-
	bagof(Annot, where_constraint_list(Goal, Annot), AltAnnots),
	sort_lol(AltAnnots, AltAnnots1),
	join_alt_annots(AltAnnots1, Annotations).

%%	where_constraint_list(+Goal, -Annotations)
%	
%	Interpret   Goal,   making   annotations   on   the   variables.
%	Backtracking yields alternative annotations  due to choicepoints
%	in Goal.

where_constraint_list(Goal, Annotations) :-
	where_constraints(Goal, AttrVars, []),
	attrs_to_terms(AttrVars, Annotations).
	

where_constraints((A,B)) --> !,
	where_constraints(A),
	where_constraints(B).
where_constraints((A;B)) --> !,
	(   where_constraints(A)
	;   where_constraints(B)
	).
where_constraints(serql_compare(like, Var, Pattern)) --> !,
	constrain(Var, like(Pattern)).
where_constraints(serql_compare(=, Var, Value)) --> !,
	constrain(Var, eq(Value)).
where_constraints(rdfql_is_literal(V)) --> !,
	constrain(V, literal).
where_constraints(rdfql_is_resource(V)) --> !,
	constrain(V, resource).
where_constraints(rdf(S,P,_)) --> !,
	constrain(S, resource),
	constrain(P, resource).
where_constraints(_) -->
	[].

constrain(Var, Cond) --> 
	{ var(Var) }, !,
	(   { get_attr(Var, where, C0) }
	->  { put_attr(Var, where, (Cond, C0)) },
	    []
	;   { put_attr(Var, where, Cond)
	    },
	    [ Var ]
	).
constrain(label(X), Cond) --> !,
	constrain(X, (literal, Cond)).
constrain(lang(X), Cond) --> !,
	constrain(X, (literal, Cond)).
constrain(datatype(X), Cond) --> !,
	constrain(X, (literal, Cond)).
constrain(_, _) -->
	[].
	
%%	join_alt_annots(+ListOfAnnotLists, -AnnotatedVars)
%	
%	ListOfAnnotLists is a list  of   alternative  annotations due to
%	choicepoints. Each annotation list represents annotations in the
%	form Var = Annotation. AnnotatedVars is a list of variables with
%	attributes representing their annotations.

join_alt_annots(LoL, Annotated) :-
	smallest_var(LoL, Var), !,
	var_annotations(Var, LoL, LoL1, Annotations0),
	sort(Annotations0, Annotations), 	% remove duplicates
	(   empty_annotations(Annotations)
	->  join_alt_annots(LoL1, Annotated)
	;   put_annotations(Annotations, Var),
	    Annotated = [Var|T],
	    join_alt_annots(LoL1, T)
	).
join_alt_annots(LoL, []) :-
	assertion(maplist(=([]), LoL)).


%%	normalise_annotation(+A0, -A)
%	
%	Create  a  normalised  version  of    an   annotation  for  easy
%	processing. Currently only deals  with   annotations  that are a
%	conjunction.

normalise_annotation(A0, A) :-
	conj_to_list(A0, L0, []),
	sort(L0, L),
	list_do_conj(L, A).

conj_to_list((A,B)) --> !,
	conj_to_list(A),
	conj_to_list(B).
conj_to_list(A) -->
	[A].

list_do_conj([], any).
list_do_conj([H], H) :- !.
list_do_conj([H|T0], (H,T)) :-
	list_do_conj(T0, T).


%%	empty_annotations(+List)
%	
%	True if there is no sensible conclusion   we  can draw using the
%	annotations found. This is often the case if multiple paths in a
%	disjunction do not deal with all   variables.  Note that this is
%	not necessarily the end of the story. We could rewrite
%	
%		A,(C1;C2) into (A,C1);(A,C2)
%		
%	And apply optimisation on both branches.

empty_annotations([]) :- !.
empty_annotations(List) :-
	memberchk(any, List).

put_annotations([], _).
put_annotations([One], Var) :- !,
	put_attr(Var, where, One).
put_annotations(More, Var) :-
	put_attr(Var, where, or(More)).

%%	smallest_var(+ListOfList, -Smallest)
%	
%	Get  the  smallest  (in  standard   order  of  terms)  annotated
%	variable.

smallest_var([[S0=_|_]|T], Smallest) :-
	smallest_var(T, S0, Smallest).
smallest_var([[]|T], Smallest) :-
	smallest_var(T, Smallest).

smallest_var([], S, S).
smallest_var([[S1=_|_]|T], S0, S) :- !,
	smallest(S1, S0, S2),
	smallest_var(T, S2, S).
smallest_var([[]|T], S0, S) :-
	smallest_var(T, S0, S).
	
smallest(A, B, S) :-
	(   A @< B
	->  S = A
	;   S = B
	).

%%	var_annotations(+Var, +LoL0, -LoL, -Annotations)
%	
%	Get all Annotation for Var. Note   that the annotation is either
%	the head of the list or not in the list.

var_annotations(_, [], [], []) :- !.
var_annotations(Var, [[Var=A|TA0]|TL0], LoL, [A|TA]) :- !,
	(   TA0 == []
	->  LoL = TL
	;   LoL = [TA0|TL]
	),
	var_annotations(Var, TL0, TL, TA).
var_annotations(Var, [A0|TL0], [A0|TL], [any|A]) :-
	var_annotations(Var, TL0, TL, A).


where:attr_unify_hook(_,_) :- fail.
where:attr_portray_hook(Val, _Var) :-
	print(Val).

%%	attrs_to_terms(AttrsVars, List)
%	
%	Convert X{where=A} into X=A terms. Without   this  we cannot use
%	bagof/3 and maintain the variables. Not sure   this  is a bug in
%	bagof or not.

attrs_to_terms([], []).
attrs_to_terms([H|T0], [H=A|T]) :-
	get_attr(H, where, A0),
	del_attr(H, where),
	normalise_annotation(A0, A),
	attrs_to_terms(T0, T).

%%	sort_lol(+ListOfList, ListOfSortedLists)

sort_lol([], []).
sort_lol([H0|T0], [H|T]) :-
	sort(H0, H),
	sort_lol(T0, T).


%%	remove_annotations(+List, +Attr)

remove_annotations([], _).
remove_annotations([H|T], A) :-
	del_attr(H, A),
	remove_annotations(T, A).


%%	object_annotations(+In, -Out, -Goal)

object_annotations(O0, O, G) :-
	get_attr(O0, where, Annotations),
	object_annot(Annotations, O0, O, G), !.
object_annotations(O, O, true).

object_annot((literal, like(Pattern)), O,
	     literal(like(Pattern), L), O = literal(L)).

%%	resource_annotations(R, G)

resource_annotations(R, Goal) :-
	get_attr(R, where, Annotations),
	resource_annot(Annotations, R, Goal), !.
resource_annotations(_, true).

resource_annot(eq(R1), R, true) :-	% where A = B
	var(R1), !,
	del_attr(R, where),
	R = R1.
resource_annot(eq(query(String)), R, true) :- !,
	del_attr(R, where),
	R = String.
resource_annot(or(List), R, Goal) :-
	eq_list(List, Resources), !,
	Goal = member(R, Resources).

eq_list([], []).
eq_list([eq(query(R))|T0], [R|T]) :-
	eq_list(T0, T).


%%	clean_conj(+Goal0, -Goal)
%	
%	Remove redundant true statements from a conjunction

clean_conj((true, G0), G) :- !,
	clean_conj(G0, G).
clean_conj((G0, true), G) :- !,
	clean_conj(G0, G).
clean_conj(G, G).

		 /*******************************
		 *	      PARSER		*
		 *******************************/

%%	serql_parse(+Input, -ParseTree)
%	
%	Parse the SeRQL statement Input into a Prolog representation.

serql_parse(Codes, Query) :-
	is_list(Codes), !,
	(   phrase(tokens(Tokens), Codes),
	    phrase(query(Query0, NameSpaces), Tokens),
	    expand_vars(Query0, Query1),
	    expand_uris(Query1, NameSpaces, Query)
	->  true
	;   syntax_error(unknown)
	).
serql_parse(Atomic, Query) :-
	atomic(Atomic), !,
	atom_codes(Atomic, Codes),
	serql_parse(Codes, Query).
serql_parse(Input, _) :-
	throw(error(type_error(text, Input), _)).


		 /*******************************
		 *	       ERRORS		*
		 *******************************/

syntax_error(What) :-
	throw(error(syntax_error(What),
		    context(_, 'in SeRQL query'))).


		 /*******************************
		 *	     NAMESPACES		*
		 *******************************/

expand_uris(Var, _, Var) :-
	var(Var), !.
expand_uris(uri(URI), _, URI) :- !.		% <!foo:bar>
expand_uris(uri(NS, URI0), Map, URI) :- !,	% foo:bar
	(   memberchk(NS=Prefix, Map)
	->  true
	;   ns(NS, Prefix)
	->  true
	;   throw(error(existence_error(namespace, NS), _))
	),
	atom_concat(Prefix, URI0, URI).
expand_uris(old_uri(NS, URI0), Map, URI) :- !,	% <foo:bar>
	(   (   memberchk(NS=Prefix, Map)
	    ;   ns(NS, Prefix)
	    )
	->  atom_concat(Prefix, URI0, URI)
	;   concat_atom([NS, :, URI0], URI)
	).
expand_uris(Q0, Map, Q) :-
	compound(Q0), !,
	functor(Q0, Name, Arity),
	functor(Q, Name, Arity),
	expand_uris(0, Arity, Q0, Map, Q).
expand_uris(Q, _, Q).

expand_uris(Arity, Arity, _, _, _) :- !.
expand_uris(I0, Arity, Q0, Map, Q) :-
	I is I0 + 1,
	arg(I, Q0, A0),
	arg(I, Q, A),
	expand_uris(A0, Map, A),
	expand_uris(I, Arity, Q0, Map, Q).


%%	ns(?Id, ?URI)
%	
%	Translate  between  namespace  id   and    URI.   If   the  flag
%	rdf_db_namespaces is true, we share   the namespace declarations
%	with the SeRQL store.

ns(NS, URI) :-
	setting(serql_parms:rdf_db_namespaces, true), !,
	rdf_db:ns(NS, URI).
ns(NS, URI) :-
	serql_ns(NS, URI).

serql_ns(rdf,  'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
serql_ns(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
serql_ns(owl,  'http://www.w3.org/2002/7/owl#').
serql_ns(xsd,  'http://www.w3.org/2001/XMLSchema#'). % Wrong in SeRQL docs!
serql_ns(serql,'http://www.openrdf.org/schema/serql#').


		 /*******************************
		 *	     VARIABLES		*
		 *******************************/

%	TBD: Check that projection variables actually appear in the
%	query!

expand_vars(select(*, Path0, Where0, Distinct, Limit, Offset),
	    select(Row, VNames, Path, Where, Distinct, Limit, Offset)) :- !,
	var_names(Path0-Where0, Path-Where, VarNames),
	vars(VarNames, Vars, Names),
	Row =.. [row | Vars],
	VNames =.. [names|Names].
expand_vars(select(Projection, Path0, Where0, Distinct, Limit, Offset),
	    select(Row, VNames, Path, Where, Distinct, Limit, Offset)) :- !,
	var_names(x(Projection,Path0,Where0), x(Vars,Path,Where), _VarNames),
	Row =.. [row | Vars],
	proj_names(Projection, Names),
	VNames =.. [names|Names].
expand_vars(construct(*, Path0, Where0, Distinct, Limit, Offset),
	    construct(Path, Path, Where, Distinct, Limit, Offset)) :- !,
	var_names(x(Path0,Where0), x(Path,Where), _VarNames).
expand_vars(construct(Ret0, Path0, Where0, Distinct, Limit, Offset),
	    construct(Ret, Path, Where, Distinct, Limit, Offset)) :- !,
	var_names(x(Ret0,Path0,Where0), x(Ret,Path,Where), _VarNames).


var_names(var(-(V)), V, _) :- !.		% bnodes, mapped from {}
var_names(var(Name), Var, Map) :-
	member(Name=Var, Map), !.
var_names(Q0, Q, Map) :-
	compound(Q0), !,
	functor(Q0, Name, Arity),
	functor(Q, Name, Arity),
	var_names(0, Arity, Q0, Q, Map).
var_names(Q, Q, _).

var_names(Arity, Arity, _, _, _) :- !.
var_names(I0, Arity, Q0, Q, Map) :-
	I is I0 + 1,
	arg(I, Q0, A0),
	arg(I, Q, A),
	var_names(A0, A, Map),
	var_names(I, Arity, Q0, Q, Map).

vars([], [], []) :- !.			% also closes list!
vars([Name=Var|T0], [Var|TV], [Name|TN]) :-
	vars(T0, TV, TN).

proj_names([], []).
proj_names([var(Var)|T0], [Var|T]) :- !,
	proj_names(T0, T).
proj_names([_|T0], [-|T]) :-
	proj_names(T0, T).


		 /*******************************
		 *	 ERROR LOCATIONS	*
		 *******************************/

syntax_error(What, In, []) :-
	throw(error(syntax_error(What),
		    context(_, left(In)))).
	
add_error_location(error(syntax_error(What),
			 context(_, left(After))),
		   Tokens) :-
	append(Before, After, Tokens),
	length(Before, BL),
	(   BL =< 5
	->  BC = Before
	;   length(BC0, 5),
	    append(_, BC0, Before),
	    BC = ['...'|BC0]
	),
	length(After, AL),
	(   AL =< 5
	->  AC = After
	;   length(AC0, 5),
	    append(AC0, _, After),
	    append(AC0, ['...'], AC)
	),
	append(BC, ['**here**'|AC], ContextTokens0),
	maplist(token_to_atom, ContextTokens0, ContextTokens),
	concat_atom(ContextTokens, ' ', Context),
	throw(error(syntax_error(What),
		    context(serql_parse/2, Context))).

token_to_atom(Token, Token) :-
	atom(Token), !.
token_to_atom(id(X), X) :- !.
token_to_atom(string(X), X) :- !.
token_to_atom(uri(URI), X) :- !,
	concat_atom([<, URI, >], X).
token_to_atom(uri(NS,Local), X) :- !,
	concat_atom([NS, Local], :, X).
token_to_atom(old_uri(NS,Local), X) :- !,
	concat_atom([<, NS, :, Local, >], X).
token_to_atom(cmp(X), X) :- !.
token_to_atom(rest(X), X) :- !.
token_to_atom(Token, Atom) :-
	term_to_atom(Token, Atom).

query(Query, NameSpaces, In, Out) :-
	catch(compilation_unit(Query, NameSpaces, In, Out),
	      E,
	      add_error_location(E, In)).

must_see(Token) -->
	[Token], !.
must_see(Token) -->
	syntax_error(expected(Token)).

must_see(Token, _) -->
	[Token], !.
must_see(_, UserName) -->
	syntax_error(expected(UserName)).


		 /*******************************
		 *	 HIGH LEVEL PARSER	*
		 *******************************/

compilation_unit(Query, NameSpaces) -->
	query(Query),
	namespace_list(NameSpaces).

%%	namespace_list(-NSList:list)// is det.
%	
%	@param NSList	List of Prefix=URI for each defined namespace

namespace_list([H|T]) -->
	[ using ], !, must_see(namespace), !,
	must_see_namespace(H),
	namespaces(T).
namespace_list([]) -->
	[].

must_see_namespace(Decl) -->
	namespace(Decl), !.
must_see_namespace(_) -->
	syntax_error(expected(namespace_declaration)).

namespace(NS=URI) -->
	must_see(id(NS), identifier),
	must_see(cmp(=), =),
	namespace_uri(URI).

namespace_uri(URI) -->
	[ uri(URI) ], !.
namespace_uri(URI) -->
	[ old_uri(Protocol, Local) ], !, % New style <foo:bar>
	{ concat_atom([Protocol, :, Local], URI)
	}.
namespace_uri(_) -->
	syntax_error(expected(absolute_uri)).

namespaces([H|T]) -->
	[ ',' ], !,
	must_see_namespace(H),
	namespaces(T).
namespaces([]) -->
	[].

query(select(Projection, Path, Where, Distinct, Limit, Offset)) -->
	[ select ], !,
	distinct(Distinct),
	projection(Projection),
	must_see(from), path_expr_list(Path),
	query_tail(Where, Limit, Offset).
query(construct(Construct, Path, Where, Distinct, Limit, Offset)) -->
	[ construct ], !,
	distinct(Distinct),
	construct_clause(Construct),
	must_see(from), path_expr_list(Path),
	query_tail(Where, Limit, Offset).
query(_) -->
	syntax_error(no_select_or_construct).

distinct(distinct) -->
	[ distinct ], !.
distinct(false) -->
	[].

query_tail(Where, Limit, Offset) -->
	(   [ where ]
	->  (   boolean_query(Where)
	    ->	[]
	    ;	syntax_error(illegal_where_clause)
	    )
	;   {Where = true}
	),
	(   [ limit ]
	->  (   pos_int(Limit)
	    ->	[]
	    ;	syntax_error(illegal_limit)
	    )
	;   {Limit = inf}
	),
	(   [ offset ]
	->  (   pos_int(Offset)
	    ->	[]
	    ;	syntax_error(illegal_offset)
	    )
	;   {Offset = 0}
	).
	
projection(*) -->
	[ * ], !.
projection([H|T]) -->
	var_or_value(H), !,
	var_or_value_list(T).
projection(_) -->
	syntax_error(expected(projection)).

construct_clause(*) -->
	[ * ], !.
construct_clause(Path) -->
	path_expr_list(Path), !.
construct_clause(_) -->
	syntax_error(expected(construct_clause)).

path_expr_list(Expr) -->
	must_see_path_expr(E0),
	(   [ ',' ]
	->  path_expr_list(Es),
	    { Expr = (E0, Es) }
	;   { Expr = E0 }
	).

must_see_path_expr(E) -->
	path_expr(E), !.
must_see_path_expr(_) -->
	syntax_error(expected(path_expression)).

path_expr(optional(_, Path)) -->
	[ '[' ], !, path_expr_list(Path), must_see(']').
path_expr(Expr) -->
	path_expr0(Expr).

path_expr0(Expr) -->
	path_expr_head(Head),
	(   (   [ ';' ]
	    ->	{ arg(1, Head, H) }
	    ;	{ arg(3, Head, H) }
	    ),
	    path_expr_tail(H, Tail)
	->  { Expr = (Head, Tail)
	    }
	;   { Expr = Head }
	).


path_expr_head(rdf(S, P, O)) -->
	must_see_node(S), must_see_edge(P), must_see_node(O).

path_expr_tail(S, Expr) -->
	[ '[' ], path_expr_tail0(S, Expr1), [ ']' ],
	{ Expr0 = optional(_, Expr1) },
	(   [ ';' ]
	->  path_expr_tail(S, Tail),
	    { Expr = (Expr0, Tail) }
	;   { Expr = Expr0 }
	).
path_expr_tail(S, Expr) -->
	path_expr_tail0(S, Expr).

%	path_expr_tail0 <=> Edge Node ((";")? Path_expr_tail)?

path_expr_tail0(S, Expr) -->
	edge(P), must_see_node(O),
	{ Statement = rdf(S, P, O) },
	(   (   [ ';' ]
	    ->  path_expr_tail(S, Tail)
	    ;	path_expr_tail(O, Tail)
	    )
	->  { Expr = (Statement, Tail) }
	;   { Expr = Statement }
	).

must_see_edge(Edge) -->
	edge(Edge), !.
must_see_edge(_) -->
	syntax_error(expected(edge)).

edge(var(Var)) -->
	[ id(Var) ], !.
edge(uri(URI)) -->
	[ uri(URI) ], !.		% <!foo:bar>
edge(uri(NS, URI)) -->
	[ uri(NS, URI) ], !.		% foo:bar
edge(old_uri(NS, URI)) -->
	[ old_uri(NS, URI) ], !.	% <foo:bar>

must_see_node(Node) -->
	node(Node), !.
must_see_node(_) -->
	syntax_error(expected(node)).

node(Node) -->
	[ '{' ], node_elem(E0), !, node_elem_list(Es), [ '}' ],
	(   {Es == []}
	->  {Node = E0}
	;   {Node = set([E0|Es])}
	).
node(var(-(_))) -->			% the _ is the variable that will
	[ '{', '}' ].			% be shared

node_elem_list([H|T]) -->
	[ ',' ], !,
	must_see_node_elem(H), 
	node_elem_list(T).
node_elem_list([]) -->
	[].

must_see_node_elem(Elem) -->
	node_elem(Elem), !.
must_see_node_elem(_) -->
	syntax_error(expected(node_element)).

node_elem(Elem) -->
	(   var(Elem)
	;   uri(Elem)
	;   literal(Elem)
	;   reified_stat(Elem)
	), !.

reified_stat(rdf(S,P,O)) -->
	node(S), must_see_edge(P), must_see_node(O).


		 /*******************************
		 *	      WHERE ...		*
		 *******************************/

boolean_query(Query) -->
	and_expr(And),
	(   [ or ],
	    boolean_query(Or)
	->  {Query = (And ; Or)}
	;   {Query = And}
	).

and_expr(Query) -->
	boolean_query0(Q0),
	(   [ and ],
	    and_expr(And)
	->  {Query = (Q0, And)}
	;   {Query = Q0}
	).

boolean_query0(Query) -->
	[ '(' ], !, boolean_query(Query), must_see(')').
boolean_query0(true) -->
	[ true ], !.
boolean_query0(fail) -->
	[ false ], !.
boolean_query0(\+(Q)) -->
	[ not ], !, boolean_query0(Q).
boolean_query0(serql_compare(Cmp, L, R)) -->
	var_or_query_value(L),
	[ cmp(Cmp) ], !,
	var_or_query_value(R).
boolean_query0(serql_compare(like, Var, String)) -->
	var_or_value(Var),		% must be var?
	[ like ], !, must_see_string(String).
boolean_query0(rdfql_is_literal(V)) -->
	[ isliteral, '(' ], !, var(V), must_see(')').
boolean_query0(rdfql_is_resource(V)) -->
	[ isresource, '(' ], !, var(V), must_see(')').
boolean_query0(_) -->
	syntax_error(expected(boolean_test)).

must_see_string(String) -->
	[ string(String) ], !.
must_see_string(_) -->
	syntax_error(expected(string)).

var_or_value_list([H|T]) -->
	[ ',' ], !,
	must_see_var_or_value(H),
	var_or_value_list(T).
var_or_value_list([]) -->
	[].

must_see_var_or_value(X) -->
	var_or_value(X), !.
must_see_var_or_value(_) -->
	syntax_error(expected(var_or_value)).

var_or_value(X) -->
	var(X), !.
var_or_value(X) -->
	value(X).

var_or_query_value(X) -->
	(   literal_value(Value)
	->  { X = query(Value)
	    }
	;   var_or_value(X)
	).

var(var(Var)) -->
	[ id(Var) ], !.

value(URI) -->
	uri(URI).
value('$null$') -->
	[ null ].
value(Literal) -->
	literal(Literal), !.
value(datatype(var(Var))) -->
	[ datatype, '(', id(Var), ')' ].
value(lang(var(Var))) -->
	[ lang, '(', id(Var), ')' ].
value(label(var(Var))) -->
	[ label, '(', id(Var), ')' ].

uri(uri(URI)) --> [uri(URI)].
uri(uri(NS, URI)) --> [uri(NS, URI)].
uri(old_uri(NS, URI)) --> [old_uri(NS, URI)].

literal(Literal) -->
	literal_value(Value),
	{ Literal = literal(Value) }.

literal_value(Lit) -->
	[ string(String) ],
	(   [@, id(Lang)]
	->  { Lit = lang(Lang, String) }
	;   [^^, URI]
	->  { Lit = type(URI, String) }
	;   { Lit = String }
	).

pos_int(I) -->
	[ int(I) ], { I >= 0 }.		% bit weird not to have >0, but this
					% is the Sesame spec


		 /*******************************
		 *	    TOKENISER		*
		 *******************************/

tokens([H|T]) -->
	blank,
	token(H), !,
	tokens(T).
tokens([]) -->
	blank.

token(uri(URI)) -->			% Old style absolute URI
	"<!", uri_codes(Codes), ">",
	{ atom_codes(URI, Codes)
	}.
token(old_uri(NS, Local)) -->		% Old style local, new style absolute
	"<", identifier(NS), ":", uri_codes(Codes), ">",
	{ atom_codes(Local, Codes)
	}.
token(string(String)) -->
	"\"", string_codes(Codes), "\"",
	{ atom_codes(String, Codes)
	}.
token(Token) -->
	identifier(Id), !,
	(   ":", identifier(Local)	% new style URI
	->  { Token = uri(Id, Local)
	    }
	;   {   downcase_atom(Id, Keyword),
		serql_keyword(Keyword)
	    ->  Token = Keyword
	    ;   Token = id(Id)
	    }
	).
token(int(Int)) -->
	digit(D0), !,
	digits(Digits),
	{ number_codes(Int, [D0|Digits])
	}.
token(cmp(Cmp)) -->
	cmp(Cmp), !.
token(^^) -->
	"^^", !.
token(Char) -->
	[C],
	{ single(C),
	  char_code(Char, C)
	}.
token(rest(Rest), In, []) :-		% catch syntax errors.
	In \== [],
	atom_codes(Rest, In).
	

single(0'*).
single(0'=).
single(0'().
single(0')).
single(0'{).
single(0'}).
single(0'[).
single(0']).
single(0'@).
single(0',).
single(0';).

%	cmp//1
%	
%	Returns Prolog comparison operators from the SeRQL ones.

cmp(=<) --> "<=".
cmp(\=) --> "!=".
cmp(>=) --> ">=".
cmp(=)  --> "=".
cmp(<)  --> "<".
cmp(>)  --> ">".


%%	uri_codes(-Codes)
%	
%	Get a URI string.  Does not check for otherwise valid syntax.
%	This could be done using library(url).

uri_codes([C0|Cs]) -->
	[C0],
	{ uri_code(C0)
	}, !,
	uri_codes(Cs).
uri_codes([]) -->
	[].

uri_code(C) :-
	code_type(C, csym), !.
uri_code(0'$).
uri_code(0'-).
uri_code(0'@).
uri_code(0'&).
uri_code(0'+).
uri_code(0'.).
uri_code(0'/).
uri_code(0'?).
uri_code(0'#).
uri_code(0'=).
uri_code(0':).
uri_code(0'~).				% officially not
uri_code(0';).
uri_code(0'{).
uri_code(0'}).


%%	string_codes(-Codes)
%	
%	Chars between "...",  Can contain \" and \\

string_codes([C0|Cs]) -->
	"\"", [C0],
	{ C0 == 0'\\ ; C0 = 0'" }, !,
	string_codes(Cs).
string_codes([]) -->
	peek(0'").
string_codes([C0|Cs]) -->
	[C0],
	string_codes(Cs).


%%	identifier(-Id)
%	
%	An SeRQL must start with a letter or an underscore ('_') and can
%	be followed by  zero  or   more  letters,  numbers, underscores,
%	dashes ('-') or dots ('.').

identifier(Id) -->
	[C0],
	{ code_type(C0, csymf) },
	id_chars(Cs),
	{ atom_codes(Id, [C0|Cs])
	}.

id_chars([C0|Cs]) -->
	[C0],
	{ code_type(C0, csym)
	; C0 == 0'.
	; C0 == 0'-
	}, !,
	id_chars(Cs).
id_chars([]) -->
	[].

digit(D) -->
	[D],
	{ code_type(D, digit) }.

digits([D0|Ds]) -->
	digit(D0), !,
	digits(Ds).
digits([]) -->
	[].

blank -->
	[C],
	{ code_type(C, space) }, !,
	blank.
blank -->
	[].

%%	serql_keyword(?Keyword)
%	
%	True if Keyword is the lowercase version if a keyword

serql_keyword(select).
serql_keyword(construct).
serql_keyword(from).
serql_keyword(where).
serql_keyword(using).
serql_keyword(namespace).
serql_keyword(true).
serql_keyword(false).
serql_keyword(not).
serql_keyword(and).
serql_keyword(or).
serql_keyword(like).
serql_keyword(label).
serql_keyword(lang).
serql_keyword(datatype).
serql_keyword(null).
serql_keyword(isresource).
serql_keyword(isliteral).
serql_keyword(sort).
serql_keyword(in).
serql_keyword(union).
serql_keyword(intersect).
serql_keyword(minus).
serql_keyword(exists).
serql_keyword(forall).
serql_keyword(distinct).		% SPEC: not in grammar
serql_keyword(limit).			% SPEC: not in grammar
serql_keyword(offset).			% SPEC: not in grammar

		 /*******************************
		 *	     DCG BASICS		*
		 *******************************/

peek(C, L, L) :-
	L = [C|_].


		 /*******************************
		 *    HUMAN READABLE MESSAGES	*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(error(syntax_error(What),
		     context(serql_parse/2, Location))) -->
	[ 'Syntax error in SeRQL query: ' ],
	explain(What), [ ' at **here** in', nl, nl],
	['~w'-[Location] ].

explain(expected(X)) -->
	[ '"~w" expected'-[X] ].
