/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(rdfql_runtime,
          [ rdfql_carthesian/1,         % +Bags

            rdfql_bind_null/1,          % +List
            rdfql_cond_bind_null/1,     % +List
            rdfql_triple_in/2,          % -Triple, +Triples

                                        % SeRQL support
            serql_compare/3,            % +Comparison, +Left, +Right
            serql_eval/2,               % +Term, -Evaluated
            serql_member_statement/2,   % -Triple, +List

                                        % SPAQRL support
            sparql_true/1,              % +Term
            sparql_eval/2,              % +Expression, -Result
            sparql_find/5,              % ?From, ?To, ?F, ?T, :Q
            sparql_minus/2,             % :Q1, :Q2
            sparql_group/1,             % :Query
            sparql_group/3,             % :Query, +OuterVars, +InnerVars
            sparql_subquery/3,          % +Proj, +Query, +Solutions
            sparql_service/5,           % +Silent, +URL, +Prefixes, +Vars, +QText
            sparql_update/1             % +Updates
          ]).
:- use_module(library(nb_set)).
:- use_module(library(debug)).
:- use_module(serql_runtime).
:- use_module(sparql_runtime).

:- meta_predicate
    rdfql_carthesian(:).

/** <module> SPARQL/SeRQL runtime support predicates

This module provides runtime support for  running compiled queries. I.e.
it defines special constructs that may be   emitted  by the compiler and
optmizer that are common  to  all   query  languages.  Language specific
runtime support is in serql_runtime.pl and sparql_runtime.pl

@see    serql_runtime.pl for the implementation of the SeRQL routines.
@see    sparql_runtime.pl for the implementation of the SPARQL routines.
*/

                 /*******************************
                 *      CARTHESIAN PRODUCT      *
                 *******************************/

%!  rdfql_carthesian(:Bags) is nondet.
%
%   Bags is a list of independent goals. This predicate provides the
%   variable bindings for the carthesian product of all solutions of
%   each goal in Bags.  For example:
%
%       ==
%       ?- rdfql_carthesian([ bag([X], between(1,2,X)),
%                             bag([Y], between(1,2,Y))]).
%       X = 1, Y = 1 ;
%       X = 1, Y = 2 ;
%       X = 2, Y = 1 ;
%       X = 2, Y = 2 ;
%       false.
%       ==

rdfql_carthesian(M:Bags) :-
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
solve_bags([bag(Templ, Goal, _Branch, _Cost)|T0], M, N, [set(Templ,Set,Size)|T]) :-
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
carthesian_select([call(Goal)|T]) :-
    call(Goal),
    carthesian_select(T).
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
                 *          NULL HANDLING       *
                 *******************************/

%!  rdfql_cond_bind_null(+List) is det.
%
%   Bind variables in List  to   our  NULL-representation,  which is
%   =|$null$|=.

rdfql_cond_bind_null([]).
rdfql_cond_bind_null([H|T]) :-
    (   var(H)
    ->  H = '$null$'
    ;   true
    ),
    rdfql_cond_bind_null(T).

%!  rdfql_bind_null(+List) is semidet.
%
%   True if all elements in List unify with =|$null$|=.

rdfql_bind_null([]).
rdfql_bind_null(['$null$'|T]) :-
    rdfql_bind_null(T).


%!  rdfql_triple_in(-Triple, +Triples) is nondet.
%
%   True when Triple is an rdf(S,P,O) element in Triples that does
%   not contain NULL.  Used for CONSTRUCT and DESCRIBE.

rdfql_triple_in(Triple, Triples) :-
    Triple = rdf(S,P,O),
    member(Triple, Triples),
    S \== '$null$',
    P \== '$null$',
    O \== '$null$'.
