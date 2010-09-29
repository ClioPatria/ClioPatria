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

:- module(rdf_bounded,
	  [ resource_CBD/3,		% :Expand, +URI, -Graph
	    graph_CBD/3,		% :Expand, +Graph0, -Graph
	    bnode_vars/3		% +Graph0, -VarGraph, -BNodeVars
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(assoc)).


/** <module> RDF Bounded descriptions

The predicates in this  module  deal   with  blank  nodes.  They provide
expansion of URIs and graphs to include  the descriptions of blank nodes
and replacing blank nodes with Prolog variables.

@tbd	Also implement the variations on CBD
@see	http://n2.talis.com/wiki/Bounded_Descriptions_in_RDF
*/

:- meta_predicate
	resource_CBD(3, +, -),
	graph_CBD(3, +, -).


%%	resource_CBD(:Expand, +URI, -Graph) is det.
%
%	Graph is the Concise Bounded Description  of URI. This notion is
%	also known as "the bnode-closure of a resource".
%
%	@param	Expand is called to enumerate the PO pairs for a subject.
%		This will often be =rdf= to use rdf/3.

resource_CBD(Expand, S, Graph) :-
	empty_assoc(Map0),
	findall(rdf(S,P,O), call(Expand, S,P,O), Graph, BNG),
	new_bnodes(Graph, Map0, Map1, BN, []),
	phrase(r_cbd(BN, Expand, Map1, _Map), BNG).

r_cbd([], _, Map, Map) -->
	[].
r_cbd([H|T], Expand, Map0, Map, Graph, Tail) :-
	rdf_is_bnode(H),
	\+ get_assoc(H, Map0, _), !,
	findall(rdf(H,P,O), call(Expand, H,P,O), Graph, Tail0),
	new_bnodes(Graph, Map0, Map1, BN, T),
	r_cbd(BN, Expand, Map1, Map, Tail0, Tail).
r_cbd([_|T], Expand, Map0, Map) -->
	r_cbd(T, Expand, Map0, Map).

new_bnodes(Var, Map, Map, BN, BN) :-
	var(Var), !.
new_bnodes([rdf(_,_,O)|RDF], Map0, Map, BN, T) :-
	rdf_is_bnode(O), !,
	(   get_assoc(O, Map0, _)
	->  new_bnodes(RDF, Map0, Map, BN, T)
	;   put_assoc(O, Map0, true, Map1),
	    BN = [O|BNT],
	    new_bnodes(RDF, Map1, Map, BNT, T)
	).
new_bnodes([_|RDF], Map0, Map, BN, T) :-
	new_bnodes(RDF, Map0, Map, BN, T).


%%	graph_CBD(:Expand, +Graph0, -Graph) is det.
%
%	Add concise bounded descriptions for bnodes in a graph, creating
%	an expanded graph.

graph_CBD(Expand, Graph0, Graph) :-
	empty_assoc(Map0),
	phrase(gr_cbd(Graph0, Expand, Map0, _Map), Graph).

gr_cbd([], _, Map, Map) -->
	[].
gr_cbd([rdf(S,P,O)|T], Expand, Map0, Map) -->
	{   rdf_is_bnode(S)
	;   rdf_is_bnode(O)
	}, !,
	[ rdf(S,P,O) ],
	r_cbd([S,O], Expand, Map0, Map1),
	gr_cbd(T, Expand, Map1, Map).
gr_cbd([Triple|T], Expand, Map0, Map) -->
	[Triple],
	gr_cbd(T, Expand, Map0, Map).


		 /*******************************
		 *        BNODE --> VAR		*
		 *******************************/

%%	bnode_vars(+RDF, -RDFWithVars, -Vars) is det.
%
%	Consistently replace bnodes in  RDF   with  Prolog  variable and
%	unify Vars with a list of the variables found.
%
%	@param  RDF is a list rdf(S,P,O)
%	@param  Resolved is a list rdf(S,P,O), where resources may be
%		a variable
%	@param	NodeIDs is a list of variables representing the bnodes.

bnode_vars(Graph0, Graph, NodeIDs) :-
	empty_assoc(Map0),		% BNodeID --> Var
	bnode_vars(Graph0, Graph, Map0, Map),
	assoc_to_values(Map, NodeIDs).

bnode_vars([], [], Map, Map).
bnode_vars([rdf(S0,P0,O0)|T0], Graph, Map0, Map) :-
	(   rdf_is_bnode(S0)
	;   rdf_is_bnode(P0)
	;   rdf_is_bnode(O0)
	), !,
	Graph = [rdf(S,P,O)|T],
	bnode_var(S0, S, Map0, Map1),
	bnode_var(P0, P, Map1, Map2),
	bnode_var(O0, O, Map2, Map3),
	bnode_vars(T0, T, Map3, Map).
bnode_vars([Triple|T0], [Triple|T], Map0, Map) :-
	bnode_vars(T0, T, Map0, Map).


bnode_var(R0, BNodeID, Map0, Map) :-
	rdf_is_bnode(R0), !,
	(   get_assoc(R0, Map0, BNodeID)
	->  Map = Map0
	;   put_assoc(R0, Map0, BNodeID, Map)
	).
bnode_var(R, R, Map, Map).
