/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2018, University of Amsterdam,
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

:- module(api_void,
          [
          ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(settings)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_wrapper)).

/** <module> Void vocabulary description of the server

@see http://www.w3.org/TR/void/
*/

:- setting(cliopatria:void_file, any, '',
           'File served for /.well-known/void').
:- setting(cliopatria:void_graph, atom, '',
           'Graph holding void data').

:- http_handler(cliopatria('.well-known/void'), handle_void,
                [id(well_known_void)]).

handle_void(Request) :-
    setting(cliopatria:void_file, File), File \== '',
    absolute_file_name(File, Path, [access(exist)]),
    !,
    http_reply_file(Path, [unsafe(true)],  Request).
handle_void(Request) :-
    setting(cliopatria:void_graph, Graph), Graph \== '',
    http_link_to_id(export_graph, graph(Graph), HREF),
    http_redirect(see_other, HREF, Request).
handle_void(_Request) :-
    findall(rdf(S,P,O), void_triple(S,P,O), Triples0),
    rdf_global_term(Triples0, Triples),
    format('Content-type: application/x-turtle; charset="UTF-8"~n~n'),
    rdf_save_turtle(stream(current_output),
                    [ expand(triple_in(Triples)),
                      only_known_prefixes(true)
                    ]).

:- public triple_in/5.

triple_in(RDF, S,P,O,_G) :-
    member(rdf(S,P,O), RDF).

%!  void_triple(+Request, -Subject, -Predicate, -Object)

void_triple('_:dataset', rdf:type, void:'DataSet').
void_triple('_:dataset', dcterms:creator,
            literal('ClioPatria')).
void_triple('_:dataset', dcterms:description,
            literal(lang(en, D))) :-
    D = 'This Void dataset description is auto-generated from \c
             the ClioPatria store content. It supports auto-discovery \c
             of the SPARQL endpoint and lists the available named graphs.\n\c
             If you want a nicer document, you can either supply a Turtle \c
             file and use the setting `cliopatria:void_file` to serve this \c
             file from `.well-known/void` or create a named graph in the \c
             RDF store and use the setting `cliopatria:void_graph`.'.
void_triple('_:dataset', void:sparqlEndpoint, EndPoint) :-
    http_current_request(Request),
    http_public_host_url(Request, Host),
    http_link_to_id(sparql_query, [], Location),
    atom_concat(Host, Location, EndPoint).
void_triple(S, P, O) :-
    rdf_graph(Graph),
    graph_dataset_uri(Graph, DataSet),
    graph_triple(Graph, DataSet, S, P, O).

graph_triple(_, DataSet, '_:dataset', void:subset, DataSet).
graph_triple(_, DataSet, DataSet, rdf:type, void:'DataSet') .
graph_triple(Graph, DataSet, DataSet, void:triples, literal(type(xsd:integer, Triples))) :-
    rdf_graph_property(Graph, triples(Count)),
    atom_number(Triples, Count).
graph_triple(Graph, DataSet, DataSet, void:dataDump, Export) :-
    http_current_request(Request),
    http_public_host_url(Request, Host),
    http_link_to_id(export_graph, [graph(Graph)], Location),
    atom_concat(Host, Location, Export).


graph_dataset_uri(Graph, Graph) :-
    !,
    uri_is_global(Graph).
graph_dataset_uri(_, BNode) :-
    rdf_bnode(BNode).
