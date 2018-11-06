/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2018, University of Amsterdam,
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

:- module(skosxl_entailment,
          [ rdf/3
          ]).
:- use_module(rdfql(rdfql_runtime)).    % runtime tests
:- use_module(library(semweb/rdf_db),
              [ rdf_global_id/2,
                rdf_subject/1,
                rdf_current_predicate/1,
                (rdf_meta)/1,
                op(_,_,_)
              ]).

/** <module> SKOSXL entailment

The function of an entailment module is  to provide an implementation of
rdf/3 that extends basic triple-lookup using the entailment rules of the
SKOS-XL entailent rules

This entailment module does only some SKOS XL inferences, see the numbers of the rules.

*/

:- rdf_meta
        rdf(r,r,o).

rdf(S, P, O) :-
    rdf_db:rdf(S, P, O).

% S53
rdf(skosxl:prefLabel,   rdf:type, owl:'ObjectProperty').
rdf(skosxl:altLabel,    rdf:type, owl:'ObjectProperty').
rdf(skosxl:hiddenLabel, rdf:type, owl:'ObjectProperty').

% S54
rdf(skosxl:prefLabel,   rdfs:range,  skosxl:'Label').
rdf(skosxl:altLabel,    rdfs:range,  skosxl:'Label').
rdf(skosxl:hiddenLabel, rdfs:range,  skosxl:'Label').

% S55
rdf(S, skos:prefLabel, O) :-
    rdf_db:rdf(S, skosxl:prefLabel, L),
    rdf_db:rdf(L, skosxl:literalForm, O).
% S56
rdf(S, skos:altLabel, O) :-
    rdf_db:rdf(S, skosxl:altLabel, L),
    rdf_db:rdf(L, skosxl:literalForm, O).
% S57
rdf(S, skos:hiddenLabel, O) :-
    rdf_db:rdf(S, skosxl:hiddenLabel, L),
    rdf_db:rdf(L, skosxl:literalForm, O).



                 /*******************************
                 *             REGISTER         *
                 *******************************/

:- multifile
    cliopatria:entailment/2.

cliopatria:entailment(skosxl, skosxl_entailment).
