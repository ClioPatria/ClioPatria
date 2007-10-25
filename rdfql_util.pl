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
	SortKey =.. [v|Vars],
	findall(SortKey-Result, Goal, Results0),
	(   Distinct == distinct
	->  sort(Results0, Results1)
	;   keysort(Results0, Results1)
	),
	apply_offset(Offset, Results1, Results2),
	apply_limit(Limit, Results2, Results),
	member(_Key-Result, Results).
	
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

apply_offset(0, List, List) :- !.
apply_offset(N, [_|T], List) :-
	N > 0,
	N2 is N - 1,
	apply_offset(N2, T, List).

apply_limit(inf, List, List) :- !.
apply_limit(0, _, []).
apply_limit(N, [H|T0], [H|T]) :-
	N > 0,
	N2 is N - 1,
	apply_offset(N2, T0, T).


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

