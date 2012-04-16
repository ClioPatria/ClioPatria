/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam,
		   VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(count,
	  [ proof_count/2,		% :Goal, -Count
	    proof_count/3,		% :Goal, +Max, -Count
	    answer_count/3,		% ?Var, :Goal, -Count
	    answer_count/4,		% ?Var, :Goal, +Max -Count
	    answer_set/3,		% ?Var, :Goal, -Answers
	    answer_set/4,		% ?Var, :Goal, +Max, -Answers
	    answer_pair_set/5,		% ?Pair, :Goal, +MaxKeys, +MaxPerKey, -Answers
	    unique_solution/2		% :Goal, -Solution
	  ]).
:- use_module(library(nb_set)).
:- use_module(library(rbtrees)).
:- use_module(library(nb_rbtrees)).


/** <module> This module provides various ways to count solutions

This module is based on a  similar   collection  introduces in the first
ClioPatria release. Most  names  have  been   changed  to  describe  the
semantics more accurately.

The  predicates  in  this  library  provide  space-efficient  solutions,
avoiding findall/setof. Most predicates come with  a variant that allows
limiting the number of answers.

@tbd	The current implementation is often based on library(nb_set), which
	implements _unbalanced_ binary trees.  We should either provide a
	balanced version or use Paul Tarau's interactors to solve these
	problems without destructive datastructures.
*/

:- meta_predicate
	proof_count(0, -),
	proof_count(0, +, -),
	answer_count(?, 0, -),
	answer_count(?, 0, +, -),
	answer_set(?, 0, -),
	answer_set(?, 0, +, -),
	answer_pair_set(?, 0, +, +, -),
	unique_solution(0, -).

%%	proof_count(:Goal, -Count) is det.
%%	proof_count(:Goal, +Max, -Count) is det.
%
%	True if Count is the number of   times  Goal succeeds. Note that
%	this is not the same as the number of answers. E.g, repeat/0 has
%	infinite  proofs  that  all  have    the   same  -empty-  answer
%	substitution.
%
%	@see answer_count/3

proof_count(Goal, Count) :-
	proof_count(Goal, infinite, Count).

proof_count(Goal, Max, Count) :-
	State = count(0),
	(   Goal,
	    arg(1, State, N0),
	    N is N0 + 1,
	    nb_setarg(1, State, N),
	    N == Max
	->  Count = Max
	;   arg(1, State, Count)
	).

%%	answer_count(?Var, :Goal, -Count) is det.
%%	answer_count(?Var, :Goal, +Max, -Count) is det.
%
%	Count number of unique answers of Var Goal produces. Enumeration
%	stops if Max solutions have been found, unifying Count to Max.

answer_count(T, G, Count) :-
	answer_count(T, G, infinite, Count).

answer_count(T, G, Max, Count) :-
	empty_nb_set(Set),
	C = c(0),
	(   G,
	    add_nb_set(T, Set, true),
	    arg(1, C, C0),
	    C1 is C0+1,
	    nb_setarg(1, C, C1),
	    C1 == Max
	->  Count = Max
	;   arg(1, C, Count)
	).

%%	answer_set(?Var, :Goal, -SortedSet) is det.
%%	answer_set(?Var, :Goal, +MaxResults, -SortedSet) is det.
%
%	SortedSet is the set of bindings for Var for which Goal is true.
%	The predicate answer_set/3 is the same  as findall/3 followed by
%	sort/2. The predicate answer_set/4  limits   the  result  to the
%	first MaxResults. Note that this is *not*  the same as the first
%	MaxResults from the  entire  answer   set,  which  would require
%	computing the entire set.

answer_set(T, G, Ts) :-
	findall(T, G, Raw),
	sort(Raw, Ts).

answer_set(T, G, Max, Ts) :-
	empty_nb_set(Set),
	State = count(0),
	(   G,
	    add_nb_set(T, Set, true),
	    arg(1, State, C0),
	    C is C0 + 1,
	    nb_setarg(1, State, C),
	    C == Max
	->  true
	;   true
	),
	nb_set_to_list(Set, Ts).

%%      answer_pair_set(Var, :Goal, +MaxKeys, +MaxPerKey, -Group)
%
%       Bounded find of Key-Value  pairs.   MaxKeys  bounds  the maximum
%       number of keys. MaxPerKey bounds the   maximum number of answers
%       per key.

answer_pair_set(P, G, MaxKeys, MaxPerKey, Groups) :-
        P = K-V,
        (   MaxPerKey = inf
        ->  true
        ;   TooMany is MaxPerKey+1,
            dif(New, values(TooMany))
        ),
        rb_empty(Tree),
        State = keys(0),
        (   G,
            add_pair(Tree, K, V, New),
            New == new_key,
            arg(1, State, C0),
            C is C0+1,
            nb_setarg(1, State, C),
            C == MaxKeys
        ->  true
        ;   true
        ),
        groups(Tree, Groups).

add_pair(T, K, V, New) :-
        nb_rb_get_node(T, K, N), !,
        nb_rb_node_value(N, NV),
        NV = k(Count, VT),
        (   nb_rb_get_node(VT, V, _)
        ->  New = false
        ;   NewCount is Count + 1,
            New = values(NewCount),
            nb_rb_insert(VT, V, true),
            nb_setarg(1, NV, NewCount)
        ).
add_pair(T, K, V, new_key) :-
        rb_one(V, true, RB),
        nb_rb_insert(T, K, k(1, RB)).

rb_one(K, V, Tree) :-
        rb_empty(T0),
        rb_insert(T0, K, V, Tree).

groups(Tree, Groups) :-
        rb_visit(Tree, Pairs),
        maplist(expand_values, Pairs, Groups).

expand_values(K-k(_Count,T), K-Vs) :-
        rb_keys(T, Vs).

%%	unique_solution(:Goal, -Solution) is semidet.
%
%	True if Goal produces exactly  one   solution  for Var. Multiple
%	solutions are compared using  =@=/2.   This  is semantically the
%	same as the code below, but  fails   early  if a second nonequal
%	solution for Var is found.
%
%	  ==
%	  findall(Var, Goal, Solutions), sort(Solutions, [Solution]).
%	  ==

unique_solution(Goal, Solution) :-
	State = state(false, _),
	(   Goal,
	    (	arg(1, State, false)
	    ->	nb_setarg(1, State, true),
		nb_setarg(2, State, Solution),
		fail
	    ;	arg(2, State, Answer),
		Answer =@= Solution
	    ->  fail
	    ;	!, fail				% multiple answers
	    )
	;   arg(1, State, true),
	    arg(2, State, Solution)
	).
