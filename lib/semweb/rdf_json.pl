/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Michiel Hildebrand
    Author:        Jan Wielemaker
    E-mail:        michielh@few.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007-2010, E-Culture/MultimediaN
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


:- module(rdf_json,
	  [ graph_json/2,		% +Graph, -JSON
	    properties_to_json/2,	% +Pairs:property-values, -JSONList
	    rdf_object_to_json/2	% +RDFObject, -JSONTerm
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

@see	http://n2.talis.com/wiki/RDF_JSON_Specification
*/

%%	graph_json(+Graph, -JSON) is det.
%
%	JSON is a Prolog JSON object representing Graph.
%
%	@param	Graph is a list of rdf(S,P,O)
%	@see	json_write/3 for serializing JSON

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

uri_key(bnode(NodeID), Key, bnode) :- !,
	atom_concat('_:', NodeID, Key).
uri_key(BNode, _, _) :-
	rdf_is_bnode(BNode), !,
	type_error(rdf_resource, BNode).
uri_key(URI, URI, uri).

%%	graph_to_json(+Pairs:property-values, -JSON) is det.
%
%	JSON is a prolog json term of grouped Pairs.

properties_to_json([], []) :- !.
properties_to_json([P-Vs|T], [P=JSON|Rest]) :-
	objects_to_json(Vs, JSON),
	properties_to_json(T, Rest).


%%	objects_to_json(+Objects:list(rdf_object), -JSON) is det.
%
%	Convert all objects in Rs to a JSON term.

objects_to_json([], []) :- !.
objects_to_json([R|T], [json(JSON)|Vs]) :-
 	rdf_object_to_json(R, JSON),
	objects_to_json(T, Vs).


%%	rdf_object_to_json(+RDFObject, -JSON) is det.
%
%	Convert an RDF Object to a JSON   term.  RDFObject can be a term
%	bnode(NodeID). RDFObject is *not* allowed to be a blank-node.
%
%	@error	type_error(rdf_resource, BNode) if RDFObject is a
%		blank node.

rdf_object_to_json(literal(Lit), Object) :- !,
	Object = [value=Txt, type=literal|Rest],
	literal_to_json(Lit, Txt, Rest).
rdf_object_to_json(URI, [value=Key, type=Type]) :-
	uri_key(URI, Key, Type).

%%	literal_to_json(+Literal, -Text, -Attributes)
%
%	Extract text and Attributes from Literal object.

literal_to_json(lang(Lang, Txt), Txt, [lang=Lang]) :- !.
literal_to_json(type(Type, Txt), Txt, [datatype=Type]) :- !.
literal_to_json(Txt, Txt, []).
