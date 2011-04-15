/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2011, University of Amsterdam,
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

:- module(rdf_schema,
	  [ rdf_graph_schema/2		% +DataGraph, +SchemaGraph
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- thread_local
	schema_triple/3.
:- multifile
	known_schema_prefix/1.

%%	rdf_graph_schema(+Graph, +SchemaTriples) is det.
%
%	Create an initial  schema  by   providing  definitions  for  all
%	predicates and types (classes) used  in   Graph.  The  schema is
%	dumped into the graph SchemaGraph.

rdf_graph_schema(Data, Schema) :-
	retractall(schema_triple(_,_,_)),
	make_schema(Data),
	findall(rdf(S,P,O), retract(schema_triple(S,P,O)), Schema).

:- rdf_meta
	assert_schema(r,r,o).

assert_schema(S,P,O) :-
	schema_triple(S,P,O), !.
assert_schema(S,P,O) :-
	assert(schema_triple(S,P,O)).


make_schema(Data) :-
	forall(predicate_in_graph(Data, P),
	       define_predicate(P, Data)),
	forall(type_in_graph(Data, Class),
	       define_type(Class)).

known_schema_prefix(rdf).
known_schema_prefix(rdfs).
known_schema_prefix(owl).
known_schema_prefix(skos).
known_schema_prefix(dc).
known_schema_prefix(dcterms).

known_url(P) :-
	known_schema_prefix(Prefix),
	rdf_global_id(Prefix:_, P), !.

define_predicate(P, _) :-
	known_url(P), !.
define_predicate(P, DataGraph) :-
	copy_data(P),
	assert_schema(P, rdf:type, rdf:'Property'),
	assign_label(P),
	predicate_statistics(DataGraph, P, _C,
			     _Subjects, _Objects,
			     Domains, Ranges),
	(   Domains = [Dom]
	->  assert_schema(P, rdfs:domain, Dom)
	;   true
	),
	(   Ranges = [Range]
	->  assert_schema(P, rdfs:range, Range)
	;   true
	).


define_type(C) :-
	known_url(C), !.
define_type(C) :-
	copy_data(C),
	assert_schema(C, rdf:type, rdfs:'Class'),
	assign_label(C).


assign_label(S) :-
	(   rdf(S, rdfs:label, _)
	->  true
	;   rdfs_label(S, Label),
	    Label \== S
	->  assert_schema(S, rdfs:label, literal(Label))
	;   true
	).


copy_data(S) :-
	retractall(schema_triple(S,_,_)),
	forall(rdf(S,P,O),
	       assert_schema(S,P,O)).


		 /*******************************
		 *	        QUERY		*
		 *******************************/

predicate_in_graph(Graph, P) :-
	rdf_current_predicate(P),
	once(rdf(_,P,_,Graph)).

%%	type_in_graph(+Graph, -Class)
%
%	Generate the unique types in Graph

:- thread_local
	type_seen/1.

type_in_graph(Graph, Class) :-
	call_cleanup(type_in_graph2(Graph, Class),
		     retractall(type_seen(_))).

type_in_graph2(Graph, Class) :-
	subject_in_graph(Graph, S),
	(   rdf(S, rdf:type, Class)
	*-> true
	;   rdf_equal(Class, rdfs:'Resource')
	),
	(   type_seen(Class)
	->  fail
	;   assert(type_seen(Class))
	).


subject_in_graph(Graph, S) :-
	rdf_subject(S),
	once(rdf(S, _, _, Graph)).

predicate_statistics(Graph, P, C, Subjects, Objects, Domains, Ranges) :-
	findall(S-O, rdf(S,P,O,Graph), Pairs),
	length(Pairs, C),
	pairs_keys_values(Pairs, Ss, Os),
	sort(Ss, Subjects),
	sort(Os, Objects),
	resources_types(Subjects, Graph, Domains),
	resources_types(Objects, Graph, Ranges).

resources_types(URIs, Graph, Types) :-
	findall(T, resource_type_in(URIs, Graph, T), TList),
	sort(TList, Types).

resource_type_in(List, Graph, T) :-
	member(URI, List),
	resource_type(URI, Graph, T).

%%	resource_type(+URI, +Graph, -Type) is det.

resource_type(URI, Graph, T) :-
	(   URI = literal(Lit)
	->  (   Lit = type(T, _)
	    ->	true
	    ;	rdf_equal(T, rdfs:'Literal')
	    )
	;   rdf(URI, rdf:type, T, Graph)
	*-> true
	;   rdf_equal(T, rdfs:'Resource')
	).
