/*  Part of ClioPatria SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2018, VU University Amsterdam
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

:- module(jena_properties, []).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(aggregate)).
:- use_module(sparql_runtime).

:- multifile
    sparql:functional_property/2,
    sparql:current_functional_property/3.

ns(apf,   'http://jena.hpl.hp.com/ARQ/property#').
ns(lists, 'http://jena.hpl.hp.com/ARQ/list#').
ns(Prefix, URI) :-
    rdf_current_ns(Prefix, URI).

alias('java:com.hp.hpl.jena.sparql.pfunction.library.',
      'http://jena.hpl.hp.com/ARQ/property#').
alias('java:com.hp.hpl.jena.query.pfunction.library.',
      'http://jena.hpl.hp.com/ARQ/property#').

property_alias(Prefix:Local, Global) :-
    ns(Prefix, URI),
    alias(AliasBase, URI),
    atom_concat(AliasBase, Local, Global).

absolute_uri(Prefix:Local, Global) :-
    ns(Prefix, URI),
    atom_concat(URI, Local, Global).

term_expansion((sparql:functional_property(S, NS:Term0) :- Body),
               [ (sparql:functional_property(S, Term) :- Body),
                 sparql:current_functional_property(P, P, Argc)
               | Aliases
               ]) :-
    Term0 =.. [Name|Args],
    length(Args, Argc),
    absolute_uri(NS:Name, P),
    Term =.. [P|Args],
    findall(sparql:current_functional_property(P1, P, Argc),
            property_alias(NS:Name, P1),
            Aliases).


                 /*******************************
                 *    JENA PROPERTY FUNCTIONS   *
                 *******************************/

% See http://jena.sourceforge.net/ARQ/library-propfunc.html

% (S apf:assign, O) is basically unification.

sparql:functional_property(S, apf:assign(O)) :-
    (   S = O
    ->  true
    ;   sparql_true(S=O)
    ).


                 /*******************************
                 *             LISTS            *
                 *******************************/

rdf_list(S) :-
    rdf_equal(S, rdf:nil).
rdf_list(S) :-
    rdf(S, rdf:first, _).

rdf_container(Container) :-
    container_class(Class),
    rdfs_individual_of(Container, Class).

:- rdf_meta container_class(r).

container_class(rdf:'Bag').
container_class(rdf:'Seq').
container_class(rdf:'Alt').

% (S, lists:member, O) means that O is a member of the collection S. In
% Jena, S may be unbound, finding all lists on the database.

sparql:functional_property(S, lists:member(O)) :-
    rdf_list(S),
    rdfs_member(O, S).

sparql:functional_property(S, rdfs:member(O)) :-
    rdf_container(S),
    rdfs_member(O, S).

sparql:functional_property(S, apf:bag(O)) :-
    nonvar(S),
    rdfs_individual_of(S, rdfs:'Bag'),
    rdfs_member(O, S).
sparql:functional_property(S, apf:seq(O)) :-
    nonvar(S),
    rdfs_individual_of(S, rdfs:'Seq'),
    rdfs_member(O, S).
sparql:functional_property(S, apf:alt(O)) :-
    nonvar(S),
    rdfs_individual_of(S, rdfs:'Alt'),
    rdfs_member(O, S).


% (S, lists:length, O) is true when O is the length of the collection S.
% Again, S may be unbound.

sparql:functional_property(S, lists:length(O)) :-
    rdf_list(S),
    aggregate_all(count, rdfs_member(_, S), Len),
    rdf_equal(xsd:integer, IntType),
    atom_number(String, Len),
    O = literal(type(IntType, String)).

sparql:functional_property(S, lists:index(literal(type(IntType, Index)),
                                          Element)) :-
    rdf_list(S),
    rdf_equal(xsd:integer, IntType),
    (   var(Index)
    ->  rdfs_nth1(I, S, Element),
        atom_number(Index, I)
    ;   atom_number(Index, I),
        rdfs_nth1(I, S, Element)
    ->  true
    ).


rdfs_nth1(0, Set, Element) :-
    rdf_has(Set, rdf:first, Element).
rdfs_nth1(I, Set, Element) :-
    var(I),
    !,
    rdf_has(Set, rdf:rest, Tail),
    rdfs_nth1(I0, Tail, Element),
    I is I0 + 1.
rdfs_nth1(I, Set, Element) :-
    I2 is I - 1,
    rdf_has(Set, rdf:rest, Tail),
    rdfs_nth1(I2, Tail, Element).


