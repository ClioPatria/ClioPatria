/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam,
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

:- module(rdf_describe,
	  [ rdf_bounded_description/4,	% :Expand, +Type, +URI, -Graph
	    resource_CBD/3,		% :Expand, +URI, -Graph
	    graph_CBD/3,		% :Expand, +Graph0, -Graph
	    rdf_include_reifications/3,	% :Expand, +Graph0, -Graph
	    rdf_include_labels/3,	% :Expand, +Graph0, -Graph
	    lcbd_label/3		% +Subject, -Pred, -Label
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(assoc)).
:- use_module(library(lists)).


/** <module> RDF Bounded descriptions

The predicates in this module deal   with  `RDF bounded descriptions'. A
bounded description is a  subgraph  that   describes  a  single resource
(URI). Unfortunately, such  an  isolated   description  is  not possible
without the possibility of loosing semantics. We provide some meaningful
approximations described in the literature.

Scanning the definitions given in  the   link  below, we distinguish two
ortogonal expansions: one expanding the graph  and another adding either
reifications    or    labels.    Expansion      is     implemented    by
rdf_bounded_description/4, while the  returned  graph   can  be  further
expanded using rdf_include_reifications/3 and/or rdf_include_labels/3.


@tbd	Also implement the variations on CBD
@see	http://n2.talis.com/wiki/Bounded_Descriptions_in_RDF
*/

:- meta_predicate
	rdf_bounded_description(3, +, +, -),
	resource_CBD(3, +, -),
	graph_CBD(3, +, -),
	rdf_include_reifications(3, +, -).


		 /*******************************
		 *     RESOURCE OPERATIONS	*
		 *******************************/

%%	rdf_bounded_description(:Expand, +Type, +URI, -Graph) is det.
%
%	Graph is a Bounded Description of   URI.  The literature defines
%	various types of  bounding   descriptions.  Currently  supported
%	types are:
%
%	    * cbd
%	    Concise Bounded Description of URI. This notion is also
%	    known as "the bnode-closure of a resource"
%	    * scbd
%	    Symmetric Concise Bounded Description is similar to
%	    =cbd=, but includes triples with both URI as subject and
%	    object.


rdf_bounded_description(Expand, Type, S, Graph) :-
	empty_assoc(Map0),
	expansion(Type, Expand, S, Graph, BNG),
	phrase(new_bnodes(Graph, Map0), BN),
	phrase(r_bnodes(BN, Type, Expand, Map0, _Map), BNG).

:- meta_predicate
	expansion(+, 3, +, -, ?),
	r_bnodes(+, +, 3, +, -, ?, ?).

expansion(cbd, Expand, S, RDF, Tail) :-
	findall(rdf(S,P,O), call(Expand, S,P,O), RDF, Tail).
expansion(scbd, Expand, S, RDF, Tail) :-
	findall(rdf(S,P,O), call(Expand, S,P,O), RDF, T0),
	findall(rdf(O,P,S), call(Expand, O,P,S), T0, Tail).

r_bnodes([], _, _, Map, Map) -->
	[].
r_bnodes([H|T], Type, Expand, Map0, Map, Graph, Tail) :-
	rdf_is_bnode(H), !,
	put_assoc(H, Map0, true, Map1),
	expansion(Type, Expand, H, Graph, Tail0),
	phrase(new_bnodes(Graph, Map1), BN, T),
	r_bnodes(BN, Type, Expand, Map1, Map, Tail0, Tail).
r_bnodes([_|T], Type, Expand, Map0, Map) -->
	r_bnodes(T, Type, Expand, Map0, Map).

new_bnodes(Var, _) -->
	{ var(Var) }, !.
new_bnodes([rdf(S,_,O)|RDF], Map) -->
	new_bnode(S, Map),
	new_bnode(O, Map),
	new_bnodes(RDF, Map).

new_bnode(S, Map) --> { rdf_is_bnode(S), \+ get_assoc(S, Map, _) }, !, [S].
new_bnode(_, _) --> [].


%%	resource_CBD(:Expand, +URI, -Graph) is det.
%
%	Graph is the Concise Bounded Description  of URI. This notion is
%	also known as "the bnode-closure  of   a  resource".  Note that,
%	according to the definition on the  Talis wiki, the CBD includes
%	reified  statements.  This  predicate  does  not  do  this.  Use
%	rdf_include_reifications/3 to add reifications to the graph.
%
%	@param	Expand is called to enumerate the PO pairs for a subject.
%		This will often be =rdf= to use rdf/3.
%	@see	http://n2.talis.com/wiki/Bounded_Descriptions_in_RDF

resource_CBD(Expand, S, Graph) :-
	rdf_bounded_description(Expand, cbd, S, Graph).


		 /*******************************
		 *	GRAPH OPERATIONS	*
		 *******************************/

%%	graph_CBD(:Expand, +Graph0, -Graph) is det.
%
%	Add concise bounded descriptions for bnodes in a graph, creating
%	an expanded graph.

graph_CBD(Expand, Graph0, Graph) :-
	empty_assoc(Map0),
	must_be(list, Graph0),
	phrase(gr_cbd(Graph0, Expand, Map0, _Map), Graph).

:- meta_predicate
	gr_cbd(+, 3, +, -, ?, ?).

gr_cbd([], _, Map, Map) -->
	[].
gr_cbd([rdf(S,P,O)|T], Expand, Map0, Map) -->
	{   rdf_is_bnode(S)
	;   rdf_is_bnode(O)
	}, !,
	[ rdf(S,P,O) ],
	r_bnodes([S,O], cbd, Expand, Map0, Map1),
	gr_cbd(T, Expand, Map1, Map).
gr_cbd([Triple|T], Expand, Map0, Map) -->
	[Triple],
	gr_cbd(T, Expand, Map0, Map).

%%	rdf_include_reifications(:Expand, +Graph0, -Graph) is det.
%
%	Include the reification of any reified statements in Graph0.

rdf_include_reifications(Expand, Graph0, Graph) :-
	phrase(reified_triples(Graph0, Expand), Statements),
	(   Statements == []
	->  Graph = Graph0
	;   graph_CBD(Expand, Statements, Statements1),
	    rdf_include_reifications(Expand, Statements1, Graph1),
	    append(Graph0, Graph1, Graph)
	).

:- meta_predicate
	reified_triples(+, 3, ?, ?),
	reification(?,?,?,3,-).

reified_triples([], _) --> [].
reified_triples([rdf(S,P,O)|T], Expand) -->
	findall(T, reification(S,P,O,Expand,T)),
	reified_triples(T, Expand).

reification(S,P,O, Expand, Triple) :-
	rdf_equal(SP, rdf:subject),
	rdf_equal(PP, rdf:predicate),
	rdf_equal(OP, rdf:object),
	call(Expand, Stmt, SP, S),
	call(Expand, Stmt, OP, O),
	call(Expand, Stmt, PP, P),
	(   Triple = rdf(Stmt, SP, S)
	;   Triple = rdf(Stmt, PP, P)
	;   Triple = rdf(Stmt, OP, O)
	).

%%	rdf_include_labels(:Expand, +Graph0, -Graph) is det.
%
%	Include missing `label' statements in   Graph0.  Expand must
%	provide label triples on
%
%	    call(Expand, S, P, O)
%
%	The  predicate  lcbd_label/3  does   this    for   the  standard
%	definition, considering the properties  rdfs:label, rdfs:comment
%	and rdfs:seeAlso.

rdf_include_labels(Expand, Graph0, Graph) :-
	phrase(label_triples(Graph0, Expand), LabelRDF),
	(   LabelRDF == []
	->  Graph = Graph0
	;   append(Graph0, LabelRDF, Graph)
	).

:- meta_predicate
	label_triples(+, 3, ?, ?),
	label_triple(+, 3, -).

label_triples([], _) --> [].
label_triples([rdf(_,_,O)|T], Expand) -->
	findall(T, label_triple(O,Expand,T)),
	label_triples(T, Expand).

label_triple(O, Expand, Triple) :-
	call(Expand, O, LP, Label),
	Triple = rdf(O, LP, Label).

:- rdf_meta
	lcbd_property(r).

%%	lcbd_label(+S, -P, -Label) is nondet.
%
%	Standard conforming `Expand' for rdf_include_labels/3.

lcbd_label(S, P, Label) :-
	lcbd_property(P),
	rdf_has(S, P, Label).

lcbd_property(rdfs:label).
lcbd_property(rdfs:comment).
lcbd_property(rdfs:seeAlso).
