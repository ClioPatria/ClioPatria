/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, University of Amsterdam,
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
	absolute_file_name(File, Path, [access(exist)]), !,
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

%%	void_triple(+Request, -Subject, -Predicate, -Object)

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


graph_dataset_uri(Graph, Graph) :- !,
	uri_is_global(Graph).
graph_dataset_uri(_, BNode) :-
	rdf_bnode(BNode).
