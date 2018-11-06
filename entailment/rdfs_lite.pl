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

:- module(rdfslite_entailment,
          [
          ]).
:- use_module(rdfql(rdfql_runtime)).    % runtime tests
:- use_module(library('semweb/rdf_db'),
              [ rdf_global_id/2,
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
                rdfs_subproperty_of/2,
                rdfs_individual_of/2
              ]).

/** <module> RDFS-Lite entailment

The function of an entailment module is  to provide an implementation of
rdf/3 that extends basic triple-lookup using the entailment rules of the
semantic web sub language of RDF.

This entailment module does the part  of   RDFS  that can be implemented
efficiently  using  backward  chaining.  Notably    it   does  *not*  do
type-reasoning on the basis of domains/ranges of properties. It does:

        * Use the property hierarchy
        * Define the serql special relations
        * Handle rdfs:subClassOf as transitive
        * Handle rdfs:subPropertyOf as transitive
        * Handle rdf:type using subProperties and rdfs:subClassOf
*/

:- rdf_meta
        rdf(o,o,o).

:- public
    rdf/3,
    rdf/4.

rdf(S, P, O) :-
    var(P),
    !,
    rdf_db:rdf(S,P,O).
rdf(S, serql:directSubClassOf, O) :-
    !,
    rdf_has(S, rdfs:subClassOf, O).
rdf(S, serql:directType, O) :-
    !,
    rdf_has(S, rdf:type, O).
rdf(S, serql:directSubPropertyOf, O) :-
    !,
    rdf_has(S, rdfs:subPropertyOf, O).
rdf(S, rdfs:subClassOf, O) :- ( atom(S) ; atom(O) ),
    !,
    rdf_reachable(S, rdfs:subClassOf, O).
rdf(S, rdfs:subClassOf, O) :-
    !,
    rdf_has(S, rdfs:subClassOf, Direct),
    rdf_reachable(Direct, rdfs:subClassOf, O).
rdf(S, rdfs:subPropertyOf, O) :-
    !,
    rdf_reachable(S, rdfs:subPropertyOf, O).
rdf(S, rdf:type, O) :-
    !,
    (   var(S), var(O)
    ->  rdf_subject(S)
    ;   true
    ),
    rdfs_individual_of(S, O).
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

                 /*******************************
                 *             REGISTER         *
                 *******************************/

:- multifile
    cliopatria:entailment/2.

cliopatria:entailment(rdfslite, rdfslite_entailment).
