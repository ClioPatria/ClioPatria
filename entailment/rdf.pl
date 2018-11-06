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

:- module(rdf_entailment,
          [ rdf/3,
            rdf/4
          ]).
:- use_module(rdfql(rdfql_runtime)).    % runtime tests
:- use_module(library(semweb/rdf_db),
              [ rdf_global_id/2,
                rdf_subject/1,
                rdf_current_predicate/1,
                (rdf_meta)/1,
                op(_,_,_)
              ]).

/** <module> RDFS-Lite entailment

The function of an entailment module is  to provide an implementation of
rdf/3 that extends basic triple-lookup using the entailment rules of the
semantic web sub language of RDF.

This entailment module does only the core RDF inferences:

    * Every subject is of type rdfs:Resource
    * Every resource that appears as a predicate is of type
      rdf:Property
*/

:- rdf_meta
        rdf(r,r,o).

rdf(S, P, O) :-
    rdf_db:rdf(S, P, O).
rdf(S, rdf:type, rdf:'Property') :-
    var_or_resource(S),
    rdf_current_predicate(S),
    (   rdf_db:rdf(_, S, _)
    ->  \+ rdf_db:rdf(S, rdf:type, rdf:'Property')
    ).
rdf(S, rdf:type, rdfs:'Resource') :-
    var_or_resource(S),
    rdf_subject(S),
    \+ rdf_db:rdf(S, rdf:type, rdfs:'Resource').
rdf(S, serql:directSubClassOf, O) :-
    !,
    rdf_db:rdf(S, rdfs:subClassOf, O).
rdf(S, serql:directType, O) :-
    !,
    rdf_db:rdf(S, rdf:type, O).
rdf(S, serql:directSubPropertyOf, O) :-
    !,
    rdf_db:rdf(S, rdfs:subPropertyOf, O).

var_or_resource(R) :-
    (   var(R)
    ->  true
    ;   atom(R)
    ).

%!  rdf(?S, ?P, ?O, ?G)

rdf(S, P, O, G) :-
    rdf_db:rdf(S, P, O, G).


                 /*******************************
                 *             REGISTER         *
                 *******************************/

:- multifile
    cliopatria:entailment/2.

cliopatria:entailment(rdf, rdf_entailment).
