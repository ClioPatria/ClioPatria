/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2012, University of Amsterdam
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

:- module(rdfql_util,
	  [ select_results/6,		% +Distinct, +Offset, +Limit,
					% :SortBy, -Result, :Goal
	    select_results/9,		% +Distinct, +Group, +Having, +Agg,
					% +Offset, +Limit,
					% :SortBy, -Result, :Goal
	    entailment_module/2		% +Entailment, -Module
	  ]).
:- use_module(library(nb_set)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(sparql_runtime).

:- meta_predicate
	select_results(+, +, 0, +, +, +, +, -, 0),
	select_results(+, +, +, +, -, 0),
	select_results(+, +, +, -, 0).

%%	select_results(+Distinct, +Offset, +Limit, +SortBy, -Result, :Goal)
%
%	Calls select_results/8 using Group=[] and Having=true.

select_results(Distinct, Offset, Limit, SortBy, Result, Goal) :-
	select_results(Distinct, [], true, [],
		       Offset, Limit, SortBy, Result, Goal).

%%	select_results(+Distinct, +Group, +Having, +Aggregates,
%%		       +Offset, +Limit, +SortBy, -Result, :Goal) is nondet.
%
%	Select results for the  template   Result  on  backtracking over
%	Goal.
%
%	@param Distinct
%	Iff 'distinct', only consider distinct solutions
%
%	@param Group
%	is a list of variables on which to group the results.  These
%	are the only variables that can be used in the HAVING filter
%	and final projection.
%
%	@param Having
%	is a constraint (similar to =FILTER=) to filter grouped
%	results.
%
%	@param Aggregates
%	List of aggregate(Function, Var)
%
%	@param Offset
%	Skip the first Offset results.  Offset is applied after
%	Distinct and SortBy
%
%	@param Limit
%	Only return the first Limit results. Limit is applied
%	after Distinct, SortBy and Offset. The value 'inf'
%	returns all values.
%
%	@param SortBy
%	Either 'unsorted' or a term order_by(Cols), where each Col
%	in Cols is a term ascending(Expr) or descending(Expr).
%
%	@tbd	Group, Having and Aggregate are currently ignored.

:- discontiguous select_results/9.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
1. ORDERED RESULTS WITHOUT AGGREGATES
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

select_results(Distinct, [], _:true, [], Offset, Limit,
	       order_by(Cols), Result, Goal) :-
	exclude(ground, Cols, SortCols), SortCols \== [], !,
	reverse(SortCols, RevSortCols),
	group_order(RevSortCols, GroupedCols),
	maplist(sort_key_goal, GroupedCols, GroupedKeys, KeyGenList),
	list_conj(KeyGenList, KeyGenGoal),
	findall(GroupedKeys-Result, (Goal,rdfql_util:KeyGenGoal), Results0),
	(   Distinct == distinct
	->  sort(Results0, Results1)
	;   Results1 = Results0
	),
	order_by(GroupedCols, Results1, Results2),
	apply_offset(Offset, Results2, Results3),
	apply_limit(Limit, Results3, Results),
	member(_Key-Result, Results).

%%	group_order(+Cols, -GroupedCols) is det.
%
%	Group ordering expressions by the same ordering direction. E.g.,
%	[ascending(X), ascending(Y), descending(Z)] becomes
%	[ascending([X,Y]), descending([Z])]

group_order([], []).
group_order([Col|CT0], [Grouped|GT]) :-
	Col =.. [Dir,Expr],
	Grouped =.. [Dir,[Expr|Exprs]],
	same_order(Dir, CT0, CT, Exprs),
	group_order(CT, GT).

same_order(Dir, [Col|CT0], CT, [E0|ET]) :-
	Col =.. [Dir,E0], !,
	same_order(Dir, CT0, CT, ET).
same_order(_, CT, CT, []).

list_conj([], true) :- !.
list_conj([G], G) :- !.
list_conj([G|T0], (G,T)) :-
	list_conj(T0, T).



%%	order_by(+Cols, +Results0, -Results) is det.
%
%	Order the results.  Cols is a list of ascending(Var) or
%	descending(Var).  We need to


order_by([], Results, Results).
order_by([ascending(_)|T], Results0, Results) :- !,
	keysort(Results0, Results1),
	(   T == []
	->  Results = Results1
	;   maplist(truncate_key, Results1, Results2),
	    order_by(T, Results2, Results)
	).
order_by([descending(_)|T], Results0, Results) :-
	keysort(Results0, Results1),
	reverse(Results1, Results2),
	(   T == []
	->  Results = Results2
	;   maplist(truncate_key, Results2, Results3),
	    order_by(T, Results3, Results)
	).

%%	truncate_key(Keyed, Truncated)
%
%	Delete one level of the  key.   Because  keysort  is stable, the
%	implied order is not changed anymore.

truncate_key([_|Key]-V, Key-V).

%%	sort_key_goal(+ColGroup, -KeyGroup, -Translate)

sort_key_goal(ColGroup, KeyGroup, Translate) :-
	arg(1, ColGroup, Exprs),
	sort_expr_goal(Exprs, KeyGroup, Translate).

sort_expr_goal([], [], true).
sort_expr_goal([V], [K], sort_key(V,K)) :- !.
sort_expr_goal([V|TV], [K|TK], (sort_key(V,K),G)) :-
	sort_expr_goal(TV, TK, G).


%%	sort_key(+Result, -Key) is det.
%
%	Determine the sort-key from a result according to the SPARQL
%	standard:
%
%	    1. undefined/null
%	    2. blank nodes
%	    3. IRIs
%	    4. RDF Literals by their plain value (simple literal)
%
%	@bug This is not good enough. Literals must be compared in their
%	value-space.  This requires some study.
%	@bug Result is a SPARQL expression.

:- public sort_key/2.			% Goal created by sort_key_goal/3.

sort_key(Var, Var) :- var(Var), !.
sort_key(literal(L), sk(4, V)) :- !,
	simple_literal(L, V).
sort_key(IRI, sk(N, IRI)) :-
	(   rdf_is_bnode(IRI)
	->  N = 2
	;   N = 3
	).

simple_literal(lang(_Lang, SL), SL).
simple_literal(type(_Type, SL), SL).
simple_literal(SL, SL).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
2. UNORDERED RESULTS WITHOUT AGGREGATES
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

select_results(Distinct, [], _:true, [], Offset, Limit,
	       _Unsorted, Result, Goal) :- !,
	select_results(Distinct, Offset, Limit, Result, Goal).

%%	select_results(+Distinct, +Offset, +Limit, -Result, :Goal)
%
%	Unsorted version. In this case we can avoid first collecting all
%	results.

select_results(distinct, Offset, Limit, Result, Goal) :- !,
	term_variables(Result, Vars),
	V =.. [v|Vars],
	empty_nb_set(Set),
	Counter = count(0),
	Goal,
	   add_nb_set(V, Set, New),
	   New == true,
	   apply_limit(Counter, Offset, Limit, Last),
	   ( Last == true -> ! ; true ).
select_results(_, 0, inf, _, G) :- !,
	G.
select_results(_, Offset, Limit, _, G) :- !,
	Counter = count(0),
	G,
	    apply_limit(Counter, Offset, Limit, Last),
	    ( Last == true -> ! ; true ).

apply_limit(_, 0, inf, false) :- !.
apply_limit(Counter, Offset, Limit, Last) :-
	arg(1, Counter, N0),
	N is N0 + 1,
	nb_setarg(1, Counter, N),
	N > Offset,
	(   Limit \== inf,
	    N =:= Offset+Limit
	->  Last = true
	;   Last = false
	).

%%	apply_offset(+Offset, +AllSolutions, -OffsetSolutions) is det.

apply_offset(N, [_|T], List) :-
	N > 0, !,
	N2 is N - 1,
	apply_offset(N2, T, List).
apply_offset(_, List, List).

%%	apply_limit(+Limit,  +AllSolutions, -LimitSolutions) is det.

apply_limit(inf, List, List) :- !.
apply_limit(Limit, All, List) :-
	limit(Limit, All, List).

limit(N, [H|T0], [H|T]) :-
	N > 0, !,
	N2 is N - 1,
	limit(N2, T0, T).
limit(_, _, []).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
3. AGGREGATE SUPPORT

To support aggregation, we have to collect   results and group them into
sets that have equal values for the  variables that appear in Group. For
each group, we must:

  1. Evaluate the Aggregate functions
  2. Evaluate the Having constraing and drop sets for which Having fails.

Note that the projection (=Result) and   Having constraints can only use
variables Group and aggregate functions. This   implies  that we need to
track the grouped variables as well  as   variables  that  are needed to
compute the aggregates, but _no_ more.

Q: Can we call select_results/9 recursively  with the iteration over the
groups to get OrderBy, Offset and Limit working?

Q: When do we need to apply Distinct? Before or after grouping?

Note that we do not need to do anything with the result term because the
output variables are already shared with it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

select_results(_Distinct, [], Having, AggregateEval, _Offset, _Limit,
	       _Order, _Result, Goal) :- !,
	aggregate_vars(AggregateEval, Aggregates, AggVars, Eval),
	AV =.. [a|AggVars],
	findall(AV, Goal, Group),
	aggregate(Group, Aggregates),
	call(Eval),
	call(Having).
select_results(_Distinct, Group, Having, AggregateEval, _Offset, _Limit,
	       _Order, _Result, Goal) :-
	aggregate_vars(AggregateEval, Aggregates, AggVars, Eval),
	GV =.. [v|Group],
	AV =.. [a|AggVars],
	findall(GV-AV, Goal, Pairs),
	keysort(Pairs, SortedPairs),
	group_pairs_by_key(SortedPairs, Groups),
	member(GV-G, Groups),
	aggregate(G, Aggregates),
	call(Eval),
	call(Having).

%%	aggregate(+Group, +Aggregates)

aggregate([AVT|Group], Aggregates) :- !,
	AVT =.. [a|AV0],
	maplist(aggregate_setup, AV0, State0),
	aggregate_steps(Group, State0, State),
	maplist(aggregate_bind, Aggregates, State).
aggregate([], Aggregates) :-
	maplist(empty_aggregate, Aggregates).

aggregate_setup(count(X), Count) :-
	aggregate_step(count(X), 0, Count).
aggregate_setup(distinct(X, _Op), Set) :-
	( X == '$null$' -> Set = [] ; Set = [X] ).
aggregate_setup(sum(X0), X) :-
	sparql_eval_raw(X0, X).
aggregate_setup(min(X0), X) :-
	sparql_eval_raw(X0, X).
aggregate_setup(max(X0), X) :-
	sparql_eval_raw(X0, X).
aggregate_setup(avg(X0), X-1) :-
	sparql_eval_raw(X0, X).
aggregate_setup(sample(X), X).
aggregate_setup(group_concat(X,_), [X]).


aggregate_steps([], State, State).
aggregate_steps([HT|T], State0, State) :-
	HT =.. [a|H],
	maplist(aggregate_step, H, State0, State1),
	aggregate_steps(T, State1, State).

aggregate_step(count(X), Count0, Count) :-
	( X == '$null$' -> Count = Count0 ; Count is Count0 + 1 ).
aggregate_step(distinct(X, _Op), S0, S) :-
	( X == '$null$' -> S = S0 ; S = [X|S0] ).
aggregate_step(sum(X), Sum0, Sum) :-
	sparql_eval_raw(X+Sum0, Sum).
aggregate_step(min(X), Min0, Min) :-
	sparql_eval_raw(min(X, Min0), Min).
aggregate_step(max(X), Min0, Min) :-
	sparql_eval_raw(max(X, Min0), Min).
aggregate_step(avg(X), Sum0-Count0, Sum-Count) :-
	sparql_eval_raw(X+Sum0, Sum),
	Count is Count0+1.
aggregate_step(sample(X), S0, S) :-
	(   S0 == '$null$'
	->  S = X
	;   S = S0
	).
aggregate_step(group_concat(X, _), S0, [X|S0]).

%%	aggregate_bind(+Aggregation, +State) is det.
%
%	@tbd: bind to error if the function does not evaluate?

aggregate_bind(aggregate(Func, Var), State) :-
	aggregate_bind(Func, Var, State).

aggregate_bind(count(_), Count, Count0) :-
	rdf_equal(xsd:integer, XSDInt),
	sparql_eval(numeric(XSDInt, Count0), Count).
aggregate_bind(sum(_), Sum, Sum0) :-
	bind_number(Sum0, Sum).
aggregate_bind(min(_), Min, Min0) :-
	bind_number(Min0, Min).
aggregate_bind(max(_), Max, Max0) :-
	bind_number(Max0, Max).
aggregate_bind(avg(_), Avg, Sum-Count) :-
	rdf_equal(xsd:integer, XSDInt),
	sparql_eval(Sum/numeric(XSDInt, Count), Avg).
aggregate_bind(sample(_), Sample, Sample).
aggregate_bind(group_concat(_, literal(Sep)), literal(Concat), Parts) :-
	maplist(text_of, Parts, Texts),
	atomic_list_concat(Texts, Sep, Concat).
aggregate_bind(distinct(_, Op), Value, Set) :-
	sort(Set, Distinct),
	aggregate_distinct(Op, Distinct, Value).

%%	aggregate_distinct(+Operation, +Set, -Value)

aggregate_distinct(count, Set, Value) :-
	rdf_equal(xsd:integer, IntType),
	length(Set, Len),
	bind_number(numeric(IntType, Len), Value).
aggregate_distinct(sum, Set, Sum) :-
	rdf_equal(xsd:integer, IntType),
	foldl(add, Set, number(IntType, 0), Sum0),
	bind_number(Sum0, Sum).
aggregate_distinct(avg, Set, Avg) :-
	aggregate_distinct(sum, Set, Sum),
	length(Set, Count),
	rdf_equal(xsd:integer, XSDInt),
	sparql_eval(Sum/numeric(XSDInt, Count), Avg).

add(X, Sum0, Sum) :-
	sparql_eval_raw(X+Sum0, Sum).

text_of(Expr, Atom) :-
	sparql_eval_raw(Expr, V0),
	raw_text(V0, Atom).

raw_text(string(X), X).
raw_text(simple_literal(X), X).


bind_number(V0, V) :-
	(   V0 = numeric(_, _)
	->  sparql_eval(V0, V)
	;   V = '$null$'
	).


:- rdf_meta empty_aggregate(t).

empty_aggregate(aggregate(count(_), literal(type(xsd:integer, '0')))).
empty_aggregate(aggregate(sum(_), literal(type(xsd:integer, '0')))).
empty_aggregate(aggregate(group_concat(_,_), literal(''))).


%%	aggregate_vars(+AggregateEval, -Aggregates, -Template, -Query) is det.
%
%	@param AggregateEval is a combination of aggregate(Expr, Var)
%	       and queries that depend on aggregation results and must
%	       therefore be executed after computing the aggregates.%
%	@param Aggregates is a list of aggregate(Expr, Var), where Expr
%	       is a plain aggregate function over a number of variables
%	       and Var is shared with the place where the aggregate is
%	       used.

aggregate_vars([], [], [], true).
aggregate_vars([aggregate(Term0, Into)|T],
	       [aggregate(Term,  Into)|AT], [Term|Templ], Q) :- !,
	x_distinct(Term0, Term),
	aggregate_vars(T, AT, Templ, Q).
aggregate_vars([Q0|T], Agg, Templ, Q) :-
	aggregate_vars(T, Agg, Templ, Q1),
	mkconj(Q1, Q0, Q).

mkconj(true, Q, Q) :- !.
mkconj(Q, true, Q) :- !.
mkconj(A, B, (A,B)).

x_distinct(Term0, Term) :-
	arg(1, Term0, Spec),
	compound(Spec),
	Spec = distinct(_),
	distinct_x(Term0, Term), !.
x_distinct(Term, Term).

distinct_x(count(distinct(X)), distinct(X, count)).
distinct_x(count(sum(X)), distinct(X, sum)).
distinct_x(count(avg(X)), distinct(X, avg)).


		 /*******************************
		 *	     ENTAILMENT		*
		 *******************************/

%%	entailment_module(+Entailment, -Module)
%
%	Find the Prolog module implementing the   entailment rules for a
%	semantic web language.

entailment_module(Entailment, Module) :-
	cliopatria:entailment(Entailment, Module), !.
entailment_module(Entailment, _) :-
	throw(error(existence_error(entailment, Entailment), _)).

:- multifile
	cliopatria:entailment/2.

