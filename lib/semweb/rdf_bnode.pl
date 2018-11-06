/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, VU University Amsterdam
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

:- module(rdf_bnode,
          [ bnode_vars/3                % +Graph0, -VarGraph, -BNodeVars
          ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(assoc)).

/** <module> RDF graph operations on bnodes

This module operates on  RDF  graphs   represented  as  rdf(S,P,O)  that
contain blank nodes  (bnodes).  Bnodes   can  be  considered existential
variables in (sub-)graph,  which  motivates   replacing  them  by Prolog
variables.
*/

%!  bnode_vars(+RDF, -RDFWithVars, -Vars) is det.
%
%   Consistently replace bnodes in  RDF   with  Prolog  variable and
%   unify Vars with a list of the  variables found. Note that, if we
%   perform matches with such graphs,   multiple variables may unify
%   to  the  same  concrete  resource.  One  might  consider  adding
%   constraints such as dif/2.
%
%   @param  RDF is a list rdf(S,P,O)
%   @param  Resolved is a list rdf(S,P,O), where resources may be
%           a variable
%   @param  NodeIDs is a list of variables representing the bnodes.

bnode_vars(Graph0, Graph, NodeIDs) :-
    empty_assoc(Map0),              % BNodeID --> Var
    bnode_vars(Graph0, Graph, Map0, Map),
    assoc_to_values(Map, NodeIDs).

bnode_vars([], [], Map, Map).
bnode_vars([rdf(S0,P0,O0)|T0], Graph, Map0, Map) :-
    (   rdf_is_bnode(S0)
    ;   rdf_is_bnode(P0)
    ;   rdf_is_bnode(O0)
    ),
    !,
    Graph = [rdf(S,P,O)|T],
    bnode_var(S0, S, Map0, Map1),
    bnode_var(P0, P, Map1, Map2),
    bnode_var(O0, O, Map2, Map3),
    bnode_vars(T0, T, Map3, Map).
bnode_vars([Triple|T0], [Triple|T], Map0, Map) :-
    bnode_vars(T0, T, Map0, Map).


bnode_var(R0, BNodeID, Map0, Map) :-
    rdf_is_bnode(R0),
    !,
    (   get_assoc(R0, Map0, BNodeID)
    ->  Map = Map0
    ;   put_assoc(R0, Map0, BNodeID, Map)
    ).
bnode_var(R, R, Map, Map).
