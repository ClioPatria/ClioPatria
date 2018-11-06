/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Michiel Hildebrand and Jan Wielemaker
    E-mail:        michielh@few.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, E-Culture/MultimediaN
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

:- module(rdf_json,
          [ graph_json/2,               % +Graph, -JSON
            properties_to_json/2,       % +Pairs:property-values, -JSONList
            rdf_object_to_json/2        % +RDFObject, -JSONTerm
          ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_describe)).
:- use_module(library(semweb/rdf_bnode)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).

/** <module> JSON Representation for RDF graphs

@see    http://n2.talis.com/wiki/RDF_JSON_Specification
*/

%!  graph_json(+Graph, -JSON) is det.
%
%   JSON is a Prolog JSON object representing Graph.
%
%   @param  Graph is a list of rdf(S,P,O)
%   @see    json_write/3 for serializing JSON

graph_json(Graph, json(JSON)) :-
    bnode_vars(Graph, Graph1, BNodes),
    bnode_ids(BNodes, 1),
    map_list_to_pairs(arg(1), Graph1, Pairs),
    keysort(Pairs, Pairs1),
    group_pairs_by_key(Pairs1, SubjectKeyed),
    maplist(json_description, SubjectKeyed, JSON).

json_description(S-RDF, Key=json(JSON)) :-
    uri_key(S, Key, _Type),
    maplist(po, RDF, POList),
    keysort(POList, POSorted),
    group_pairs_by_key(POSorted, PList),
    properties_to_json(PList, JSON).

po(rdf(_,P,O), P-O).

bnode_ids([], _).
bnode_ids([bnode(N)|T], N) :-
    N2 is N + 1,
    bnode_ids(T, N2).

uri_key(bnode(NodeID), Key, bnode) :-
    !,
    atom_concat('_:', NodeID, Key).
uri_key(BNode, _, _) :-
    rdf_is_bnode(BNode),
    !,
    type_error(rdf_resource, BNode).
uri_key(URI, URI, uri).

%!  graph_to_json(+Pairs:property-values, -JSON) is det.
%
%   JSON is a prolog json term of grouped Pairs.

properties_to_json([], []) :- !.
properties_to_json([P-Vs|T], [P=JSON|Rest]) :-
    objects_to_json(Vs, JSON),
    properties_to_json(T, Rest).


%!  objects_to_json(+Objects:list(rdf_object), -JSON) is det.
%
%   Convert all objects in Rs to a JSON term.

objects_to_json([], []) :- !.
objects_to_json([R|T], [json(JSON)|Vs]) :-
    rdf_object_to_json(R, JSON),
    objects_to_json(T, Vs).


%!  rdf_object_to_json(+RDFObject, -JSON) is det.
%
%   Convert an RDF Object to a JSON   term.  RDFObject can be a term
%   bnode(NodeID). RDFObject is *not* allowed to be a blank-node.
%
%   @error  type_error(rdf_resource, BNode) if RDFObject is a
%           blank node.

rdf_object_to_json(literal(Lit), Object) :-
    !,
    Object = [value=Txt, type=literal|Rest],
    literal_to_json(Lit, Txt, Rest).
rdf_object_to_json(URI, [value=Key, type=Type]) :-
    uri_key(URI, Key, Type).

%!  literal_to_json(+Literal, -Text, -Attributes)
%
%   Extract text and Attributes from Literal object.

literal_to_json(lang(Lang, Txt), Txt, [lang=Lang]) :- !.
literal_to_json(type(Type, Txt), Txt, [datatype=Type]) :- !.
literal_to_json(Txt, Txt, []).
