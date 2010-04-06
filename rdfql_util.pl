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

:- module(rdfql_util,
	  [ select_results/6,		% +Distinct, +Offset, +Limit,
					% 	:SortBy, -Result, :Goal
	    entailment_module/2		% +Entailment, -Module
	  ]).
:- use_module(library(nb_set)).

:- module_transparent
	select_results/6,
	select_results/5.

%%	select_results(+Distinct, +Offset, +Limit, :SortBy, -Result, :Goal)
%
%	Select results for the  template   Result  on  backtracking over
%	Goal.
%
%	@param Distinct
%	Iff 'distinct', only consider distinct solutions
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
%	Either 'unsorted' or the name of a predicate that sorts
%	the result-set by calling call(SortBy, UnSorted, Sorted)

select_results(Distinct, Offset, Limit, unsorted, Result, Goal) :- !,
	select_results(Distinct, Offset, Limit, Result, Goal).
select_results(Distinct, Offset, Limit, order_by(Cols), Result, Goal) :- !,
	term_variables(Cols, Vars),
	sort_key_goal(Vars, Keys, KeyGen),
	SortKey =.. [v|Keys],
	findall(SortKey-Result, (Goal,rdfql_util:KeyGen), Results0),
	(   Distinct == distinct
	->  sort(Results0, Results1)
	;   Results1 = Results0
	),
	keysort(Results1, Results2),
	apply_offset(Offset, Results2, Results3),
	apply_limit(Limit, Results3, Results),
	member(_Key-Result, Results).

sort_key_goal([], [], true).
sort_key_goal([V], [K], sort_key(V,K)) :- !.
sort_key_goal([V|TV], [K|TK], (sort_key(V,K),G)) :-
	sort_key_goal(TV, TK, G).


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


		 /*******************************
		 *	     ENTAILMENT		*
		 *******************************/

%%	entailment_module(+Entailment, -Module)
%
%	Find the Prolog module implementing the   entailment rules for a
%	semantic web language.

entailment_module(Entailment, Module) :-
	serql:entailment(Entailment, Module), !.
entailment_module(Entailment, _) :-
	throw(error(existence_error(entailment, Entailment), _)).

:- multifile
	serql:entailment/2.

