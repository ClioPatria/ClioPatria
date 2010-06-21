/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

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

:- module(rdfql_runtime,
	  [ rdfql_carthesian/1,		% +Bags
	    rdfql_is_literal/1,		% +Node
	    rdfql_is_resource/1,	% +Node

	    rdfql_bind_null/1,		% +List
	    rdfql_cond_bind_null/1,	% +List

					% SeRQL support
	    serql_compare/3,		% +Comparison, +Left, +Right
	    serql_eval/2,		% +Term, -Evaluated
	    serql_member_statement/2,	% -Triple, +List
	    
					% SPAQRL support
	    sparql_true/1,		% +Term
	    sparql_eval/2		% +Expression, -Result
	  ]).
:- use_module(library(nb_set)).
:- use_module(library(debug)).
:- use_module(serql_runtime).
:- use_module(sparql_runtime).

:- meta_predicate
	rdfql_carthesian(:).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module provides runtime support for  running compiled queries. I.e.
it defines special constructs that may be   emitted  by the compiler and
optmizer that are common  to  all   query  languages.  Language specific
runtime support is in serql_runtime.pl and sparql_runtime.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *      CARTHESIAN PRODUCT	*
		 *******************************/

rdfql_carthesian(Carthesian) :-
	strip_module(Carthesian, M, Bags),
	solve_bags(Bags, M, 1, Sets),
	(   debugging(carthesian_size)
	->  solution_set_size(Sets, Size),
	    debug(carthesian_size, 'Total size = ~D; NO select', [Size])
	;   true
	),
	(   debugging(carthesian_no_select)
	->  true
	;   carthesian_select(Sets)
	).

solve_bags([], _, _, []).
solve_bags([bag(Vars, Goal)|T0], M, N, [set(Templ,Set,Size)|T]) :-
	Templ =.. [v|Vars],
	empty_nb_set(Set),
	(   M:Goal,
	    add_nb_set(Templ, Set),
	    fail
	;   true
	),
	size_nb_set(Set, Size),
	debug(carthesian_bags, 'Bag ~d: solution size = ~D', [N, Size]),
	Size > 0,
	N2 is N + 1,
	solve_bags(T0, M, N2, T).

carthesian_select([]).
carthesian_select([set(Templ,Set,_)|T]) :-
	gen_nb_set(Set, Templ),
	carthesian_select(T).

solution_set_size([], 0).
solution_set_size([set(_,_,Len)|T], Size) :-
	(   T == []
	->  Size = Len
	;   solution_set_size(T, Size0),
	    Size is Len * Size0
	).


		 /*******************************
		 *	    NULL HANDLING	*
		 *******************************/

rdfql_cond_bind_null([]).
rdfql_cond_bind_null([H|T]) :-
	(   var(H)
	->  H = '$null$'
	;   true
	),
	rdfql_cond_bind_null(T).


rdfql_bind_null([]).
rdfql_bind_null(['$null$'|T]) :-
	rdfql_bind_null(T).


rdfql_is_literal(literal(_)).
rdfql_is_resource(X) :-
	atom(X).
