/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
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

:- module(rdf_optimise,
	  [ rdf_optimise/1,		% :Query
	    rdf_optimise/2,		% +Query, -Optimised
	    rdf_optimise/4,		% +Query, -Optimised, -Space, -Time
	    rdf_complexity/3,		% :Goal, -SpaceEstimate, -TimeEstimate
	    serql_select_bind_null/2	% +Goal, -WithBind
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(ordsets)).
:- use_module(library(ugraphs)).

:- meta_predicate
	rdf_optimise(0).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Queries  as  returned  by  serql_compile_path/2    consists  of  a  path
expression which is compiled to a conjunction  of calls to rdf/3 and the
translation of the WHERE clause acting as an additional filter.

Optimisation of a query basically means moving conditions into the rdf/3
calls where possible, moving other conditions   as  early as possibly in
the graph-matching and reordering graph matching calls (rdf/3) to reduce
non-determinism.

Reordering
----------

Reordering of graph expressions  is   required  to  reduce backtracking.
Roughly I see three approaches:

	* Learning
	Create permutations of the query and make them run under time
	constraints.  Try to learn patterns that work (fast).

	* Use statistics
	Given the number of solutions on a certain partially instantiated
	rdf/3 call (and the required execution time), reorganise them to
	minimalise the cost.

	* Use constraint solving
	Instead of trying to solve an rdf/3 call, create a constraint from
	it and only try to solve it if there is more information.  This
	is especially attractive if some form of high-level reasoning
	from the language entailment rules can be applied or set-theory
	is a possibility.

After experiments with using constraint solving,   this  module now uses
planning based on statistical information provided by the rdf_db module.
This  algorithm  reaches  optimal  performance  under  quite  reasonable
assumptions  while  the  planning  overhead   is  very  reasonable.  The
algorithm is described in "An  optimised   Semantic  Web  query language
implementation        in        Prolog",          available         from
http://www.swi-prolog.org/download/publications/ICLP05-SeRQL.pdf

NOTES:

	* SeRQL LIKE works on resources *and* literals.  Do we want this?
	  See http://rdf4j.org/forum/mvnforum/viewthread?thread=275
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- multifile
	user:goal_expansion/2.

user:goal_expansion(rdf_complexity(G0, C), rdf_complexity(G, C)) :-
	expand_goal(G0, G).
user:goal_expansion(rdf_optimise(G0, O), rdf_optimise(G, O)) :-
	expand_goal(G0, G).
user:goal_expansion(rdf_optimise(G0, O, C, E), rdf_optimise(G, O, C, E)) :-
	expand_goal(G0, G).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Plan (conjunctions)

	* Generate permutations and costs.  Select cheapest
	* The above moves tests right after the RDF call filling
	  its input argument.  As a last optimisation some of these
	  searches may be integrated in the rdf/3 call to help using
	  indexing of the RDF database.

complexity/2 needs to update the order of clauses inside meta calls
(notably optional path expressions).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	rdf_optimise(:Goal) is nondet.
%
%	Optimise Goal and execute the result. Semantically equivalent to
%	call/1.
%
%	@tbd	Module handling is not correct.

rdf_optimise(Module:Goal) :-
	rdf_optimise(Goal, Optimised),
	call(Module:Optimised).


%%	rdf_optimise(+Goal, -Optimized) is det.
%
%	Goal is a Prolog control-structure with   calls to the rdf_db.pl
%	and  SeRQL  runtime  predicates.  Optimized  is  a  semantically
%	equivalent goal, obtained by re-ordering   conjunctions  in Goal
%	and  processing  sub-queries  that  do    not   share  variables
%	independently.

rdf_optimise(Conj, Optimised) :-
	rdf_optimise(Conj, Optimised, _, _).

rdf_optimise(Conj, Optimised, Space, Time) :-
	debug(rdf_optimise, '*** OPTIMIZING ***~n', []),
	dbg_portray_body(Conj),
	term_variables(Conj, Vars),
	rdf_complexity(Conj, Conj, S0, E0),
	State = state(Vars-Conj, S0, E0, 1),
	debug(rdf_optimise, 'C0 = ~w~n', [E0]),
	(   reorder(Conj, Perm),
	    (	debugging(rdf_optimise(all))
	    ->	dbg_portray_body(Perm)
	    ;	true
	    ),
	    rdf_complexity(Perm, Perm1, S, C),
	    debug(rdf_optimise(all), '--> space=~w, time=~w~n', [S, C]),

	    arg(4, State, N),
	    N2 is N + 1,
	    nb_setarg(4, State, N2),

	    (	arg(3, State, C0),
		C < C0
	    ->	debug(rdf_optimise,
		      'BETTER ONE [~D]: --> space=~w, time=~w~n', [N, S, C]),
		dbg_portray_body(Perm1),
	        nb_setarg(3, State, C),
		nb_setarg(2, State, S),
		nb_setarg(1, State, Vars-Perm1)
	    ;	true
	    ),
	    fail
	;   arg(1, State, Vars-Optimised),
	    arg(2, State, Space),
	    arg(3, State, Time),
	    debug(rdf_optimise, '  --> optimised: s/t = ~w/~w --> ~w/~w~n',
		  [S0, E0, Space, Time]),
	    dbg_portray_body(Optimised)
	), !.
optimise_order(Conj, Conj, -1, -1) :-
	debug(rdf_optimise, 'Failed to optimise:~n', []),
	dbg_portray_body(Conj).


		 /*******************************
		 *	     REORDERING		*
		 *******************************/

%%	reorder(Goal, Reordered)
%
%	Assuming Goal is  a  conjunction,   create  permutations  of its
%	order. Instead of blindly generating   permutations  however, we
%	get an arbitrary element of  the   conjunction  to the front and
%	compute subgraphs of goals connected   through variables _after_
%	executing the first goal.

reorder(Goal, Reordered) :-
	State = bindings([]),
	conj_to_list(Goal, Conj0),
	reorder_conj(Conj0, State, Conj1),
	list_to_conj(Conj1, Reordered),
	arg(1, State, Bindings),
	unbind(Bindings).

reorder_conj([One], _, [One]) :- !.
reorder_conj(List, State, Perm) :-
	group_by_cut(List, Before, Cut, After), !,
	reorder_conj(Before, State, PermBefore),
	bind_args(Before, State),		% this part is done
	reorder_conj(After, State, PermAfter),
	append(PermBefore, [Cut|PermAfter], Perm).
reorder_conj(List, State, Perm) :-
	group_by_optional(List, Normal, Optional), !,
	reorder_conj(Normal, State, PermNormal),
	bind_args(Normal, State),		% this part is done
	reorder_conj(Optional, State, PermOptional),
	append(PermNormal, PermOptional, Perm).
reorder_conj(List, State, [goal(_,independent(SubPerms),_)]) :-
	make_subgraphs(List, SubGraphs),
	SubGraphs \= [_], !,
	reorder_subgraph_conjs(SubGraphs, State, SubPerms).
reorder_conj(List, State, [Prefix|Perm]) :-
	select(Prefix, List, Rest),
	bind_args(Prefix, State),
	make_subgraphs(Rest, SubGraphs),
	(   SubGraphs = [SubGraph]
	->  reorder_conj2(SubGraph, State, Perm)
	;   Perm = [goal(_,independent(SubPerms),_)],
	    reorder_subgraph_conjs(SubGraphs, State, SubPerms)
	).


%%	reorder_subgraph_conjs(SubGraphs, -ReorderedSubGraphs)
%
%	Reorder  the  individual  subgraphs.  As    we  know  these  are
%	independent there is no need to  order the subgraphs themselves.
%	we also know they are  fully   connected,  and no longer contain
%	cuts, so we use the simplified  version of reorder_conj/2 called
%	reorder_conj2/2.

reorder_subgraph_conjs([], _, []).
reorder_subgraph_conjs([H0|T0], State, [H|T]) :-
	reorder_conj2(H0, State, H1),
	list_to_conj(H1, H),
	reorder_subgraph_conjs(T0, State, T).

reorder_conj2([One], _, [One]) :- !.
reorder_conj2(List, State, [Prefix|Perm]) :-
	select(Prefix, List, Rest),
	bind_args(Prefix, State),
	make_subgraphs(Rest, SubGraphs),
	(   SubGraphs = [SubGraph]
	->  reorder_conj2(SubGraph, State, Perm)
	;   Perm = [goal(_,independent(SubPerms),_)],
	    reorder_subgraph_conjs(SubGraphs, State, SubPerms)
	).

%%	group_by_cut(+List, -BeforeCut, -Cut, -AfterCut)
%
%	Split the conjunction over a  cut   (!)  as  we cannot guarantee
%	proper behaviour when moving goals to the other side of a cut.

group_by_cut(Goals, Before, Cut, After) :-
	Cut = goal(_, !, _),
	append(Before, [Cut|After], Goals), !.


%%	group_by_optional(+List, -NotOptional, -Optional)
%
%	Split the conjunction in optional and non-optional part as we
%	always want the optional part to happen last.

group_by_optional(List, Normal, Optional) :-
	split_optional(List, Normal, Optional),
	Normal \== [],
	Optional \== [].

split_optional([], [], []).
split_optional([H|T0], Normal, Optional) :-
	(   optional(H)
	->  Optional = [H|T],
	    split_optional(T0, Normal, T)
	;   Normal = [H|T],
	    split_optional(T0, T, Optional)
	).

optional(G) :-
	goal(G, (_*->_;_)).

%%	bind_args(Goal, !State)
%
%	Bind all arguments in  Goal  or   list  of  goals.  Assumes that
%	executing a goal grounds all its arguments.   Only  the goal A =
%	literal(B), generated by optimising where constraints is handled
%	special.
%
%	State is a term bindings(List)  that is destructively maintained
%	by instantiate/4.

bind_args([], _) :- !.
bind_args([H|T], State) :- !,
	bind_args(H, State),
	bind_args(T, State).
bind_args(H, State) :-
	goal(H, A=literal(B)),
	var(A), var(B), !,
	(   instantiated(A, I),
	    I \== (-)
	->  instantiate(B, _, I, State)
	;   instantiated(B, I),
	    I \== (-)
	->  instantiate(A, _, I, State)
	;   true
	).
bind_args(Goal, State) :-
	vars(Goal, Vars),
	ground_vars(Vars, State).

ground_vars([], _).
ground_vars([H|T], State) :-
	instantiate(H, _, r, State),
	ground_vars(T, State).


%%	make_subgraphs(+Goals, -SubGraphs)
%
%	Create a list of connected subgraphs   from  Goals, assuming the
%	variables in the assoc Grounded have been bound.
%
%	@param Goals is a list goal(Id, Goal, Vars).

make_subgraphs([Goal], [[Goal]]) :- !.
make_subgraphs([G1,G2], Graphs) :- !,
	unbound_vars(G1, V1),
	unbound_vars(G2, V2),
	(   ord_intersect(V1, V2)
	->  Graphs = [[G1,G2]]
	;   Graphs = [[G1],[G2]]
	).
make_subgraphs(Goals, SubGraphs) :-
	map_list_to_pairs(unbound_vars, Goals, UnBoundKeyed),
	connected_pairs(UnBoundKeyed, Edges),
	vertices_edges_to_ugraph(Goals, Edges, UGraph),
	connected_vertices(UGraph, SubGraphs).

connected_pairs([], []).
connected_pairs([H|T], Edges) :-
	connected_pairs(T, H, Edges, EdgeTail),
	connected_pairs(T, EdgeTail).

connected_pairs([], _, Edges, Edges).
connected_pairs([H|T], To, Edges, EdgeTail) :-
	(   connected(H, To, GH, GT)
	->  Edges = [GH-GT,GT-GH|Edges1],
	    connected_pairs(T, To, Edges1, EdgeTail)
	;   connected_pairs(T, To, Edges, EdgeTail)
	).

connected(V1-G1, V2-G2, G1, G2) :-
	ord_intersect(V1, V2).

connected_vertices([], []) :- !.
connected_vertices(UGraph, [Set1|Sets]) :-
	UGraph = [V1-_|_],
	reachable(V1, UGraph, Set1),
	del_vertices(UGraph, Set1, UGraph2),
	connected_vertices(UGraph2, Sets).


%%	unbound_vars(+Goal, -Vars) is det.
%
%	True when Vars is an ordered set of unbound variables in Goal.

unbound_vars(Goal, Vars) :-
	vars(Goal, AllVars),
	unbound(AllVars, Vars0),
	sort(Vars0, Vars).

unbound([], []).
unbound([H|T0], [H|T]) :-
	instantiated(H, -), !,
	unbound(T0, T).
unbound([_|T0], T) :-
	unbound(T0, T).

%%	conj_to_list(+Conj, -List)
%
%	Translate a conjunction into a list of elements of the format
%
%%		goal(Id, Goal, Vars)
%
%	Where Id is a goal identifier, Goal  is the goal itself and Vars
%	is a list of variables inside the  Goal. Variables are sorted to
%	the standard order of terms.

conj_to_list(Conj, List) :-
	phrase(conj_to_list(Conj, 1, _), List).

conj_to_list((A,B), N0, N) --> !,
	conj_to_list(A, N0, N1),
	conj_to_list(B, N1, N).
conj_to_list(true, N, N) --> !,
	[].
conj_to_list(G, N0, N) -->
	{ term_variables(G, Vars0),
	  sort(Vars0, Vars),
	  N is N0 + 1
	},
	[ goal(N0, G, Vars)
	].


%%	list_to_conj(+List, -Conj)


list_to_conj([], true).
list_to_conj([goal(_,G,_)], G) :- !.
list_to_conj([goal(_,G,_)|T0], (G,T)) :-
	list_to_conj(T0, T).


%%	id(G, Id) is det.
%%	goal(G, Goal) is det.
%%	vars(G, Vars) is det.
%
%	Extract fields from the goal structure.

%id(goal(Id, _, _),     Id).
goal(goal(_, Goal, _), Goal).
vars(goal(_, _, Vars), Vars).


		 /*******************************
		 *	       BINDING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Keep track of binding status of a   variable.  There are basically three
ways. One is to use a seperate   (assoc) table. Alternatively we can use
attributed variables and delete the attributes   at the end, and finally
we can use normal variables and recreate  the original goal by unbinding
them again.

Using assocs requires us to pass  these   things  around  and involves a
log(N) complexity lookup. Using real terms  has the disadvantage that to
unbind we have to copy the term, thus  loosing bindings it may have with
the environment.  Using attributes suffers neither of these problems and
its only drawback is relying on non-standard Prolog features.

Note that the remainder of the  algorithm   uses  sets  organised to the
standard order of terms.  As  putting   attributes  does  not change the
identity of global stack variables and goals are global stack terms this
is guaranteed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	instantiate_obj(+Arg, -Old, +New, !Bindings) is det.
%
%	Called to change the state of an   RDF object after running some
%	goal. Old is the current instantiation.  After executing the RDF
%	Goal the new instantiation is New (typically =b=). Bindings is a
%	term  bindings(List),  which  is    updated   using  destructive
%	assignment. List is the list of all  variables to which we added
%	attributes.

instantiate_obj(Arg, Old, New, Bindings) :-
	(   var(Arg)
	;   ground(Arg)
	), !,
	instantiate(Arg, Old, New, Bindings).
instantiate_obj(literal(Pattern, Var), +(Pattern), New, Bindings) :-
	instantiate(Var, _, New, Bindings).

instantiate(Var, Old, New, Bindings) :-
	instantiated(Var, Old),
	(   Old == (-)
	->  put_attr(Var, instantiated, New),
	    arg(1, Bindings, B0),
	    setarg(1, Bindings, [Var|B0])
	;   true
	).

instantiated(Term, How) :-
	(   nonvar(Term)
	->  How = +(+)
	;   get_attr(Term, instantiated, H)
	->  How = +(H)
	;   How = -
	).

uninstantiate(Term, How) :-
	(   get_attr(Term, instantiated, How)
	->  del_attr(Term, instantiated)
	;   true
	).

%%	instantiate_unify(A, B, State) is det.
%
%	We encounter a plain unification. Ideally, we determine the most
%	specific binding and propagate this and execute the unification.
%	The latter however must be undone  if we evaluate a disjunction.
%	For now, the code is somewhat   simplified  and we merely decide
%	that if one  side  is  instantiated,   the  other  will  be too,
%	disregarding the actual value we might know.

instantiate_unify(A, B, State) :-
	instantiated(B, +(_)), !,
	instantiate(A, _, b, State).
instantiate_unify(A, B, State) :-
	instantiated(A, +(_)), !,
	instantiate(B, _, b, State).
instantiate_unify(_, _, _).


%%	attr_unify_hook(+Attribute, +Value)
%
%	For now, the attribute unify hook   only  allows unifying with a
%	variable  with  the  same  attribute.    This   deals  with  the
%	unification that takes place in rdf_optimise/3 for the variables
%	of the saved copy.

instantiated:attr_unify_hook(Attr, Value) :-
	get_attr(Value, instantiated, Attr).

instantiated:attr_portray_hook(Value, _Var) :-
	write(+(Value)).

%%	instantiate_term(+Term)
%
%	Instantiate all unbound variables

instantiate_term(Term, How) :-
	compound(Term), !,
	functor(Term, _, Arity),
	instantiate_args(Arity, Term, How).
instantiate_term(Term, How) :-
	(   var(Term),
	    \+ get_attr(Term, instantiated, _)
	->  put_attr(Term, instantiated, How)
	;   true
	).

instantiate_args(0, _, _) :- !.
instantiate_args(N, Term, How) :-
	arg(N, Term, A),
	instantiate_term(A, How),
	N2 is N - 1,
	instantiate_args(N2, Term, How).


%%	uninstantiate_term(+Term, +How)
%
%	Remove all skolem instantiations from Term.

uninstantiate_term(Term, How) :-
	compound(Term), !,
	functor(Term, _, Arity),
	uninstantiate_args(Arity, Term, How).
uninstantiate_term(Term, How) :-
	uninstantiate(Term, How).

uninstantiate_args(0, _, _) :- !.
uninstantiate_args(N, Term, How) :-
	arg(N, Term, A),
	uninstantiate_term(A, How),
	N2 is N - 1,
	uninstantiate_args(N2, Term, How).


%%	delete_instantiated(+List, -Unbound)
%
%	Delete all elements of List that are not instantiated (i.e. var
%	and without an instantiated attribute).

delete_instantiated([], []).
delete_instantiated([H|T0], L) :-
	(   instantiated(H, -)
	->  L = [H|T],
	    delete_instantiated(T0, T)
	;   delete_instantiated(T0, L)
	).


		 /*******************************
		 *	    COMPLEXITY		*
		 *******************************/

%%	rdf_complexity(+GoalIn, -GoalOut, -SpaceEstimate, -TimeEstimate)
%
%	Provide an estimate  for  the  size   of  the  search  space for
%	executing Goal. For this we  estimate   the  branching factor of
%	each subgoal in the conjunction. If   the  branching factors are
%	B0, B1, ... then the total complexity estimate is
%
%		E = 1 + B0 + B0*B1 + B0*B1*B2, ...
%
%	Non-RDF calls are supposed  to  be   boolean  tests  that can be
%	executed at the first opportunity all arguments are bound by RDF
%	calls. They have a probability of   failure, reducing the search
%	space. Using the above formula, any   number  lower than 1 moves
%	the test as far as possible to the head of the conjunction.
%
%	If GoalIn and GoalOut are the same   the  system will not try to
%	optimize local conjunctions.
%
%	ISSUES: control structures ;, if-then-else, etc.

rdf_complexity(Goal, SpaceEstimate, TimeEstimate) :-
	rdf_complexity(Goal, Goal, SpaceEstimate, TimeEstimate).

rdf_complexity(Goal0, Goal, Space, Time) :-
	State = bindings([]),
	complexity(Goal0, Goal, State, 1, Space, 0, Time),
	arg(1, State, Bindings),
	unbind(Bindings).

unbind([]).
unbind([H|T]) :-
	del_attr(H, instantiated),
	unbind(T).

%	complexity(:GoalIn, -GoalOut,
%		   +State,
%		   +SpaceIn, -SpaceOut,
%		   +CountIn, -CountOut)
%
%	Compute the complexity of Goal.  Vars   is  an assoc holding the
%	variables bound earlier in the conjunction. Space keeps the size
%	of the search space and Count is   the  cummulative count of the
%	costs for exploring the search space  espressed in the number of
%	nodes that will be visited.
%
%	The (G*->_=true;_=false) clause deals with   the  code generated
%	from optional graph specs as provided by SeRQL.

complexity((A0,B0), (A,B), State, Sz0, Sz, C0, C) :- !,
	complexity(A0, A, State, Sz0, Sz1, C0, C1),
	complexity(B0, B, State, Sz1, Sz, C1, C).
complexity((G0*->True;False),
	   ( G*->True;False), State, Sz0, Sz, C0, C) :- !,
	(   var(G)
	->  optimise_order(G0, G, Sz1, C1),
	    Sz is Sz0 * Sz1,
	    C is C0+Sz0*C1
	;   complexity(G, G, State, Sz0, Sz, C0, C)
	).
complexity((If0->Then0;Else0),		% dubious
	   ( If->Then; Else), State, Sz0, Sz, C0, C) :- !,
	(   var(If)
	->  optimise_order(If0,   If,   Sz1, C1),
	    optimise_order(Then0, Then, Sz2, C2),
	    optimise_order(Else0, Else, Sz3, C3),
	    Sz is max(Sz0 * Sz1 * Sz2, Sz0 * Sz3),
	    C  is C0 + max(Sz0*C1+Sz0*Sz1*C2, Sz0*C1+Sz0*Sz1*C3)
	;   complexity(If,   If,   State, Sz0, Sz1, C0, C1),
	    complexity(Then, Then, State, Sz1, Sz2, C1, C2),
	    complexity(Else, Else, State, Sz0, Sz3, C0, C3),
	    Sz is max(Sz2, Sz3),
	    C  is max(C2, C3)
	).
complexity((A0;B0), (A;B), State, Sz0, Sz, C0, C) :- !,
	(   var(A)
	->  optimise_order(A0, A, _, _),	% First try the cheap one?
	    optimise_order(B0, B, _, _)
	;   A = A0,
	    B = B0
	),
	complexity(A, A, State, Sz0, SzA, C0, CA),
	complexity(B, B, State, Sz0, SzB, C0, CB),
	Sz is SzA + SzB,
	C is CA + CB.
complexity(sparql_group(G0), sparql_group(G), State, Sz0, Sz, C0, C) :- !,
	(   var(G)
	->  rdf_optimise(G0, G, Sz1, C1),
	    Sz is Sz0 * Sz1,
	    C is C0+Sz0*C1
	;   complexity(G, G, State, Sz0, Sz, C0, C)
	).
complexity(sparql_group(G0, OV, IV),
	   sparql_group(G, OV, IV),
	   State, Sz0, Sz, C0, C) :- !,
	(   var(G)
	->  rdf_optimise(G0, G, Sz1, C1),
	    Sz is Sz0 * Sz1,
	    C is C0+Sz0*C1
	;   complexity(G, G, State, Sz0, Sz, C0, C)
	).
complexity(rdfql_carthesian(Bags),
	   rdfql_carthesian(Bags), State, Sz0, Sz, C0, C) :- !,
	carth_complexity(Bags, State, Sz0, Sz, C0, 0, C).
complexity(independent(Goals0), Goal, State, Sz0, Sz, C0, C) :- !,
	independent_complexity(Goals0, Goal, State, Sz0, Sz, C0, 0, C).
complexity(Goal, Goal, State, Sz0, Sz, C0, C) :-
	Goal = member(Var, List), !,	% List is list of resources
	instantiate(Var, _, b, State),
	length(List, Branch),
	Sz is Sz0 * Branch,
	C is C0 + Sz0*0.2 + Sz*0.2.
complexity(Goal, Goal, State, Sz, Sz, C0, C) :-
	Goal = (A=B), !,
	instantiate_unify(A, B, State),
	C is C0 + 0.2.
complexity(Goal, Goal, State, Sz, Sz, C0, C) :-
	Goal = (Var=literal(V)), !,
	instantiated(V, +(_)),
	instantiate(Var, _, b, State),
	C is C0 + 0.2.
complexity(Goal, Goal, State, Sz0, Sz, C0, C) :-
	rdf_db_goal(Goal, S, P, O), !,
	instantiate(S, SI, b, State),
	instantiate(P, PI, b, State),
	instantiate_obj(O, OI, b, State),
	complexity0(SI, PI, OI, P, Goal, SetUp, PerAlt, Branch),
	Sz is Sz0 * Branch,
	C is C0 + Sz0*SetUp + Sz*PerAlt,
	debug(rdf_optimise(complexity), 'Complexity ~p: (~w) ~w --> ~w',
	      [i(SI,PI,OI), Goal, Branch, C]).
complexity(sparql_eval(E,V), sparql_eval(E,V), _, Sz0, Sz, C0, C) :- !,
	term_variables(E, Vars),
	all_bound(Vars),
	Sz is Sz0,			% probability of failure
	C is C0 + Sz*20.		% Sz * CostOfEval
complexity(sparql_true(E), sparql_true(E), _, Sz0, Sz, C0, C) :- !,
	term_variables(E, Vars),
	all_bound(Vars),
	Sz is Sz0 * 0.5,		% probability of failure
	C is C0 + Sz0*20.		% Sz * CostOfEval
complexity(G, G, _, Sz0, Sz, C0, C) :-	% non-rdf tests
	term_variables(G, Vars),
	all_bound(Vars),
	Sz is Sz0 * 0.5,		% probability of failure
	C is C0 + Sz0.			% Sz * CostOfTest

all_bound([]).
all_bound([H|T]) :-
	instantiated(H, +(_)),
	all_bound(T).

%%	carth_complexity(+Bags, +State,
%%			 +Size0, -Size,
%%			 +Time0, +TimeSum0, -TimeSum) is det.
%
%	Compute the time and space efficiency of the carthesian product.
%	the total cost in time is the sum  of the costs of all branches,
%	The search space at the end is still the same product.

carth_complexity([], _, Sz, Sz, _, C, C).
carth_complexity([bag(_,G)|T], State,
		 Sz0, Sz,
		 C0, Csum0, Csumz) :-
	complexity(G, G, State, Sz0, Sz1, C0, C1),
	Csum1 is Csum0 + C1,
	carth_complexity(T, State, Sz1, Sz, C0, Csum1, Csumz).


%%	independent_complexity(+GoalsIn, -Goals, +State,
%%			       +Size0, -Size,
%%			       +Time0, +TimeSum0, -TimeSum) is det.
%
%	Compute the complexity of an independent conjunction.
%
%	@param Goals is a list of g(Goal, Branch, Cost), where Branch is
%	the number of solutions expected fromGoal and Cost is the
%	estimated CPU time.

independent_complexity(GoalsIn, Goal, State,
		       Size0, Size,
		       Time0, TimeSum0, TimeSum) :-
	indep_complexity(GoalsIn, Goals1, State,
			 Size0, Size,
			 Time0, TimeSum0, TimeSum),
	keysort(Goals1, ByCost),
	pairs_values(ByCost, Goals2),
	simplify_carthesian(Goals2, Goal).

indep_complexity([], [], _, Sz, Sz, _, C, C).
indep_complexity([G0|GT0], [SzG-bag(Vars,G,SzG,CG)|GT], State,
		       Sz0, Sz,
		       C0, Csum0, Csumz) :-
	complexity(G0, G, State, Sz0, Sz1, C0, C1),
	term_variables(G, VList),
	Vars =.. [v|VList],
	(   Sz0 =:= 0
	->  SzG = 0,
	    CG = 0
	;   SzG is Sz1/Sz0,
	    CG is (C1-C0)/Sz0
	),
	Csum1 is Csum0 + C1,
	indep_complexity(GT0, GT, State, Sz1, Sz, C0, Csum1, Csumz).

%%	simplify_carthesian(+Bags, -Goal) is det.
%
%	Peer of the leading little branching   goals from the carthesian
%	handler. If there is a bag of one   left,  turn it into a normal
%	goal.

simplify_carthesian([], true).
simplify_carthesian([bag(_,Goal,_Branch,_Cost)], Goal) :- !.
simplify_carthesian([bag(_,Goal,Branch,_Cost)|Bags], Final) :-
	(   Branch < 1.5
	->  !, Final = (Goal, Final1),
	    simplify_carthesian(Bags, Final1)
	;   Bags == []
	->  !, Final = Goal
	).
simplify_carthesian(Bags, rdfql_carthesian(Bags)).


%%	complexity0(+SI, +PI, +OI, +P, +G, -Setup, -PerAlt, -Branch).
%
%	SI,PI,OI describe the instantiation pattern.  P is the predicate
%	and G is the actual goal. Complexity   is unified to an estimate
%	of the size of the search space and therefore an estimate of the
%	execution time required to prove the goal.
%
%	Literal `like' matches come  out   as  +(like(Pattern)). We must
%	estimate the percentage of the literals that match this pattern.
%	Suppose the factor is 1,000. This means the branching is reduced
%	by 1,000, but finding each solution  is   slow  as it requires a
%	linear scan. It is faster than going  all the way back to Prolog
%	backtracking however, so we estimate a   factor  10 (TBD: verify
%	that number).
%
%	ISSUES: rdf_has/3 vs rdf_reachable/3.

complexity0(+(_),+(_),+(_), _, _, 1, 0, 1) :- !.
complexity0(+(b),+(+),-, P, G, 1, 1, B) :- !,
	subj_branch_factor(G, B, Prop),
	rdf_predicate_property(P, Prop).
complexity0(-,+(+),+(b), P, G, 1, 1, B) :- !,
	obj_branch_factor(G, B, Prop),
	rdf_predicate_property(P, Prop).
complexity0(+(b), -, -, _, _, 1, 1, B) :- !,
	rdf_statistics(triples(Total)),
	subject_count(Subjs),
	(   Total == 0
	->  B = 0
	;   B is Total/Subjs
	).
complexity0(_,_,+(like(Pat)),_, G, Factor, Factor, B) :- !,
	rdf_estimate_complexity(G, B0),
	pattern_filter(Pat, Factor0),
	Factor is max(1, min(B0, Factor0)/10),
	B is B0/Factor.
complexity0(_,_,_, _, G, 1, 1, B) :-
	rdf_estimate_complexity(G, B).

:- if(rdf_statistics(subjects(_))).
subject_count(Count) :-				% RDF-DB 2.x
	rdf_statistics(subjects(Count)).
:- else.
subject_count(Count) :-				% RDF-DB 3.x
	rdf_statistics(resources(Count)).
:- endif.

:- multifile
        subj_branch_factor/3,
        obj_branch_factor/3.

subj_branch_factor(rdf(_,_,_),           X, rdf_subject_branch_factor(X)).
subj_branch_factor(rdf_has(_,_,_),       X, rdfs_subject_branch_factor(X)).
subj_branch_factor(rdf_reachable(_,_,_), X, rdfs_subject_branch_factor(X)).

obj_branch_factor(rdf(_,_,_),            X, rdf_object_branch_factor(X)).
obj_branch_factor(rdf_has(_,_,_),        X, rdfs_object_branch_factor(X)).
obj_branch_factor(rdf_reachable(_,_,_),  X, rdfs_object_branch_factor(X)).


%%	rdf_db_goal(+Goal, -Subject, -Predicate, -Object)
%
%	True if Goal is a pure (logical)   predicate on the RDF database
%	involving the given  Subject,  Predicate   and  Object.  Defined
%	multifile, allowing the  optimiser   to  understand user-defined
%	rdf/3 like predicates.
%
%	@tbd	Allow specifying different costs and branching factors

:- multifile
	rdf_db_goal/4.

rdf_db_goal(rdf(S,P,O),			S,P,O).
rdf_db_goal(rdf_has(S,P,O),		S,P,O).
rdf_db_goal(rdf_reachable(S,P,O),	S,P,O).
rdf_db_goal(rdf(S,P,O, _DB),		S,P,O). % TBD: less hits

%%	pattern_filter(+Like, -Factor)
%
%	Estimate the efficiency of a pattern. This is a bit hard, as
%	characters are not independent.

pattern_filter(Like, Factor) :-
	atom_codes(Like, Codes),
	pattern_factor(Codes, 1, Factor).

pattern_factor([], F, F).
pattern_factor([0'*|T], F0, F) :- !,
	pattern_factor(T, F0, F).
pattern_factor([_|T], F0, F) :-
	F1 is F0*10,
	pattern_factor(T, F1, F).


%%	rdf_estimate_complexity(+Goal, -Complexity)
%
%	Estimate the branching factor introduced by  Goal. This uses the
%	rdf_db  statistics  of  the  hash  chains  which  are  based  on
%	exploiting the RDFS subPropertyOf reasoning.
%
%	In addition, rdf_reachable/3 introduces its own complexity which
%	must be estimate using the branching factor of the relation.

rdf_estimate_complexity(G, C) :-
	rdf_db_goal(G, S, P0, O),
	map_predicate(P0, P),
	rdf_estimate_complexity(S, P, O, C).

map(map_predicate(_,_)).
map(map_predicate(_,_):-_).

term_expansion(In, Out) :-
	map(In), !,
	rdf_global_term(In, Out).

map_predicate(X, X) :-
	var(X), !.
map_predicate(serql:directSubClassOf,    rdfs:subClassOf) :- !.
map_predicate(serql:directType,          rdf:type) :- !.
map_predicate(serql:directSubPropertyOf, rdfs:subPropertyOf) :- !.
map_predicate(X, X).


		 /*******************************
		 *     INSTANTIATE OPTIONAL	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
In  SELECT  queries,  optional  parts  of   the  path  expression  leave
uninstantiated variables. These must be bound to  '$null$' to be able to
do correct merging for  DISTINCT.  The  naive   way  to  do  this  is to
instantiate all variables at the end  of   the  query.  On large selects
(i.e. involving many variables) this appears to be quite costly.  Doing
the job early, as in

	(   Goal
	*-> true
	;   bind_null(VarsInGoal)
	)

is not correct as well, as VarsInGoal may  be involved in other parts of
the code either before or after the optional path expression. So we need
to:

	* Do abtract execution and find the bindings done before arriving
	  at Goal.
	* continue the execution, and watch for new bindings to these
	  variables.  If we find a binding for the second time, remove
	  it from the first and make a conditional binding for it.

If we bind an argument unconditionally, we place an attribute 'b'. If it
is conditionally bound, we place an attribute   c(Set), where Set is the
set in which it was bound or plain  'c' if it was conditionally bound in
multiple places.

TBD:	disjunctions and other control structures.
	queries that do not bind (such as SPARQL bound(X))
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

serql_select_bind_null(Goal0, Goal) :-
	State = bindings([]),
	select_bind_null(Goal0, Goal1, State),
	arg(1, State, Bindings),
	c_unbind(Bindings, Left),
	(   Left == []
	->  Goal = Goal1
	;   Goal = (Goal1, rdfql_cond_bind_null(Left))
	).

c_unbind([], []).
c_unbind([H|T0], L) :-
	(   get_attr(H, instantiated, c)
	->  L = [H|T]
	;   L = T
	),
	del_attr(H, instantiated),
	c_unbind(T0, T).


select_bind_null((A0,B0), (A,B), State) :- !,
	select_bind_null(A0, A, State),
	select_bind_null(B0, B, State).
select_bind_null((G0 *-> true; true),
		 ( G *-> true; Bind),
		 State) :- !,
	arg(1, State, B0),
	select_bind_null(G0, G, State),
	arg(1, State, B),
	Bind = rdfql_bind_null(Vars),
	c_bindings(B, B0, c(Bind), Vars).
select_bind_null(rdfql_carthesian(List0),
		 rdfql_carthesian(List), State) :- !,
	select_carth_bind_null(List0, List, State).
select_bind_null(sparql_group(A0), sparql_group(A), State) :- !,
	select_bind_null(A0, A, State).
select_bind_null(sparql_group(A0,Outer,Inner),
		 sparql_group(A, Outer,Inner),
		 State) :- !,
	select_bind_null(A0, A, State),
	term_variables(Outer, Vars),
	c_bind(Vars, State).
select_bind_null(Goal, Goal, State) :-
	term_variables(Goal, Vars),
	c_bind(Vars, State).

%%	c_bindings(+AtEnd, +AtStart, +CVars, -Vars)
%
%	The  variables  of  the    difference-list   AtEnd..AtStart  are
%	conditionally bound. Tag each such  variable with CVars.
%
%	@param CVars	Term c(Vars), where Vars are the other variables
%			that have the same conditional binding.

c_bindings(B, B0, _, Vars)  :-
	B == B0, !,
	Vars = [].
c_bindings([H|T0], B0, Attr, [H|Vars]) :-
	get_attr(H, instantiated, I),
	is_instantiated(I), !,
	put_attr(H, instantiated, Attr),
	c_bindings(T0, B0, Attr, Vars).
c_bindings([_|T0], B0, Attr, Vars) :-
	c_bindings(T0, B0, Attr, Vars).


is_instantiated(b).		% unconditionally bound
is_instantiated(c(_)).		% bound either by call or rdfql_bind_null/1


%%	c_bind(+Vars, +State)
%
%	Do  unconditional  binding  of   Vars.   Var    may   be   in  a
%	rdfql_bind_null/1 set. In that case, delete it from the set, and
%	set the class to 'c' to make a conditional binding at the end.

c_bind([], _).
c_bind([H|T], State) :-
	(   get_attr(H, instantiated, I)
	->  (   I == b			% already unconditionally bound
	    ->	true
	    ;	I = c(Set)
	    ->	arg(1, Set, Vars0),
		del_var(H, Vars0, Vars),
		setarg(1, Set, Vars),
		put_attr(H, instantiated, c)
	    ;	I == c
	    ->	true
	    )
	;   put_attr(H, instantiated, b),
	    arg(1, State, B0),
	    setarg(1, State, [H|B0])
	),
	c_bind(T, State).

del_var(H, [X|T0], T) :-
	(   H == X
	->  T = T0
	;   T = [X|T1],
	    del_var(H, T0, T1)
	).

select_carth_bind_null([], [], _).
select_carth_bind_null([bag(Vars, G0)|T0], [bag(Vars, G)|T], State) :-
	select_bind_null(G0, G, State),
	select_carth_bind_null(T0, T, State).


		 /*******************************
		 *	  DEBUG SUPPORT		*
		 *******************************/

dbg_portray_body(G) :-
	debugging(rdf_optimise), !,
	portray_body(G).
dbg_portray_body(_).

portray_body(G) :-
	(   pp_instantiate_term(G),
	    debug(_, '~@',
		  [ portray_clause(current_output, (<> :- G), [module(sparql_runtime)])
		  ]),
	    fail
	;   true
	).

pp_instantiate_term(Term) :-
	compound(Term), !,
	functor(Term, _, Arity),
	pp_instantiate_args(Arity, Term).
pp_instantiate_term(Term) :-
	var(Term),
	get_attr(Term, instantiated, H), !,
	del_attr(Term, instantiated),
	Term = +(H).
pp_instantiate_term(_).

pp_instantiate_args(0, _) :- !.
pp_instantiate_args(N, Term) :-
	arg(N, Term, A),
	pp_instantiate_term(A),
	N2 is N - 1,
	pp_instantiate_args(N2, Term).


		 /*******************************
		 *	      SANDBOX		*
		 *******************************/

:- multifile sandbox:safe_meta_predicate/1.

sandbox:safe_meta_predicate(rdf_optimise:rdf_optimise/1).
