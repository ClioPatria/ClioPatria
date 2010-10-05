/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, VU University Amsterdam

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

:- module(rdf_bnode,
	  [ bnode_vars/3		% +Graph0, -VarGraph, -BNodeVars
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(assoc)).

/** <module> RDF graph operations on bnodes

This module operates on  RDF  graphs   represented  as  rdf(S,P,O)  that
contain blank nodes  (bnodes).  Bnodes   can  be  considered existential
variables in (sub-)graph,  which  motivates   replacing  them  by Prolog
variables.
*/

%%	bnode_vars(+RDF, -RDFWithVars, -Vars) is det.
%
%	Consistently replace bnodes in  RDF   with  Prolog  variable and
%	unify Vars with a list of the  variables found. Note that, if we
%	perform matches with such graphs,   multiple variables may unify
%	to  the  same  concrete  resource.  One  might  consider  adding
%	constraints such as dif/2.
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
