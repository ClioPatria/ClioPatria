/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam,
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

:- module(rdfs_entailment,
          [
          ]).
:- use_module(rdfql(rdfql_runtime)).    % runtime tests
:- use_module(library(nb_set)).
:- use_module(library('semweb/rdf_db'),
              [ rdf_current_predicate/1,
                rdf_global_id/2,
                rdf_reachable/3,
                rdf_has/3,
                rdf_has/4,
                rdf_subject/1,
                rdf_equal/2,
                (rdf_meta)/1,
                op(_,_,_)
              ]).
:- use_module(library('semweb/rdfs'),
              [ rdfs_subclass_of/2,
                rdfs_subproperty_of/2
              ]).

/** <module> RDFS entailment

The function of an entailment module is  to provide an implementation of
rdf/3 that extends basic triple-lookup using the entailment rules of the
semantic web sub language of RDF.

This entailment module does RDFS entailment.

@tbd    Check the completeness
*/

:- rdf_meta
        rdf(o,o,o),
        individual_of(r,r).

:- public
    rdf/3,
    rdf/4.

rdf(literal(L), _, _) :-                % should move to compiler
    nonvar(L), !, fail.
rdf(_, literal(L), _) :-                % idem
    nonvar(L), !, fail.
rdf(S, P, O) :-
    var(P),
    !,
    (   rdf_db:rdf(S,P,O)
    ;   rdf(P, rdf:type, rdf:'Property'),
        rdf(S, P, O),
        \+ rdf_db:rdf(S,P,O)
    ).
rdf(S, P, C) :-
    rdf_reachable(rdf:type, rdfs:subPropertyOf, P),
    !,
    individual_of(S, C).
rdf(S, P, O) :-                                 % transitive predicates
    rdf_reachable(rdfs:subClassOf, rdfs:subPropertyOf, P),
    !,
    (   (nonvar(S) ; nonvar(O))
    ->  rdfs_subclass_of(S, O)              % generate from given start
    ;   individual_of(S, rdfs:'Class'),     % generated unbounded (slow!)
        rdfs_subclass_of(S, O)
    ).
rdf(S, rdfs:subPropertyOf, O) :-
    !,
    (   nonvar(S)
    ->  individual_of(S, rdf:'Property'),
        rdfs_subproperty_of(S, O)
    ;   nonvar(O)
    ->  individual_of(O, rdf:'Property'),
        rdfs_subproperty_of(S, O)
    ;   individual_of(S, rdf:'Property'),
        rdfs_subproperty_of(S, O)
    ).
rdf(S, serql:directSubClassOf, O) :-
    !,
    rdf_has(S, rdfs:subClassOf, O).
rdf(S, serql:directType, O) :-
    !,
    rdf_has(S, rdf:type, O).
rdf(S, serql:directSubPropertyOf, O) :-
    !,
    rdf_has(S, rdfs:subPropertyOf, O).
rdf(S, P, O) :-
    rdf_has(S, P, O).

%!  rdf(?S, ?P, ?O, ?G)

rdf(S, P, O, G) :-
    var(P),
    !,
    rdf_db:rdf(S, P, O, G).
rdf(S, P, O, G) :-
    rdf_has(S, P, O, RP),
    rdf_db:rdf(S, RP, O, G).


%!  individual_of(?Resource, ?Class)

individual_of(Resource, Class) :-
    nonvar(Resource),
    !,
    (   Resource = literal(_)
    ->  rdfs_subclass_of(Class, rdfs:'Literal')
    ;   rdfs_has_type(Resource, MyClass),
        rdfs_subclass_of(MyClass, Class)
    ;   rdf_equal(Class, rdfs:'Resource')
    ).
individual_of(Resource, Class) :-
    nonvar(Class),
    !,
    (   rdf_equal(Class, rdfs:'Resource')
    ->  rdf_subject(Resource)
    ;   rdfs_subclass_of(SubClass, Class),
        rdfs_has_type(Resource, SubClass)
    ).
individual_of(Resource, Class) :-
    rdf_subject(Resource),
    individual_of(Resource, Class).


%!  rdfs_has_type(+Resource, -Class) is nondet.
%!  rdfs_has_type(-Resource, +Class) is nondet.
%
%   Perform RDFS entailment rules to enumerate the types of Resource
%   or generate all resources entailed  by   the  given  class.

rdfs_has_type(Resource, Class) :-
    empty_nb_set(Set),
    (   atom(Resource)
    ->  (   rdf_has(Resource, rdf:type, Class)
        ;   rdf_db:rdf(Resource, P, _),
            rdf_has(P, rdfs:domain, Class)
        ;   rdf_db:rdf(_, P, Resource),
            rdf_has(P, rdfs:range, Class)
        ;   rdf_db:rdf(Resource, rdfs:subPropertyOf, _),
            rdf_equal(Class, rdf:'Property')
        ;   rdf_db:rdf(_, rdfs:subPropertyOf, Resource),
            rdf_equal(Class, rdf:'Property')
        ;   (   rdf_db:rdf(_,Resource,_)
            ->  rdf_equal(Class, rdf:'Property'),
                \+ rdf_has(Resource, rdf:type, Class)
            )
        ),
        add_nb_set(Class, Set, New),
        New == true
    ;   atom(Class)
    ->  (   rdf_has(Resource, rdf:type, Class)
        ;   rdf_has(P, rdfs:domain, Class),
            rdf_has(Resource, P, _)
        ;   rdf_has(P, rdfs:range, Class),
            rdf_has(_, P, Resource)
        ;   (   rdf_reachable(Class, rdfs:subClassOf, rdf:'Property')
            ->  rdf_current_predicate(Resource),
                (   rdf(_,Resource,_)
                ->  \+ rdf_has(Resource, rdf:type, Class)
                )
            )
        ),
        add_nb_set(Resource, Set, New),
        New == true
    ;   throw(error(instantiation_error, _))
    ).


                 /*******************************
                 *             REGISTER         *
                 *******************************/

:- multifile
    cliopatria:entailment/2.

cliopatria:entailment(rdfs, rdfs_entailment).
