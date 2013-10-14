/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2010, University of Amsterdam,
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


:- module(rdfslite_entailment,
	  [
	  ]).
:- use_module(rdfql(rdfql_runtime)).	% runtime tests
:- use_module(library('semweb/rdf_db'),
	      [ rdf_global_id/2,
		rdf_reachable/3,
		rdf_has/3,
		rdf_subject/1,
		rdf_equal/2,
		(rdf_meta)/1,
		op(_,_,_)
	      ]).
:- use_module(library('semweb/rdfs'),
	      [ rdfs_subclass_of/2,
		rdfs_subproperty_of/2,
		rdfs_individual_of/2
	      ]).

/** <module> RDFS-Lite entailment

The function of an entailment module is  to provide an implementation of
rdf/3 that extends basic triple-lookup using the entailment rules of the
semantic web sub language of RDF.

This entailment module does the part  of   RDFS  that can be implemented
efficiently  using  backward  chaining.  Notably    it   does  *not*  do
type-reasoning on the basis of domains/ranges of properties. It does:

	* Use the property hierarchy
	* Define the serql special relations
	* Handle rdfs:subClassOf as transitive
	* Handle rdfs:subPropertyOf as transitive
	* Handle rdf:type using subProperties and rdfs:subClassOf
*/

:- rdf_meta
	rdf(o,o,o).

:- public
	rdf/3.

rdf(S, P, O) :-
	var(P), !,
	rdf_db:rdf(S,P,O).
rdf(S, serql:directSubClassOf, O) :- !,
	rdf_has(S, rdfs:subClassOf, O).
rdf(S, serql:directType, O) :- !,
	rdf_has(S, rdf:type, O).
rdf(S, serql:directSubPropertyOf, O) :- !,
	rdf_has(S, rdfs:subPropertyOf, O).
rdf(S, rdfs:subClassOf, O) :- ( atom(S) ; atom(O) ), !,
	rdf_reachable(S, rdfs:subClassOf, O).
rdf(S, rdfs:subClassOf, O) :- !,
       rdf_has(S, rdfs:subClassOf, Direct),
       rdf_reachable(Direct, rdfs:subClassOf, O).
rdf(S, rdfs:subPropertyOf, O) :- !,
	rdf_reachable(S, rdfs:subPropertyOf, O).
rdf(S, rdf:type, O) :- !,
	(   var(S), var(O)
	->  rdf_subject(S)
	;   true
	),
	rdfs_individual_of(S, O).
rdf(S, P, O) :-
	rdf_has(S, P, O).


		 /*******************************
		 *	       REGISTER		*
		 *******************************/

:- multifile
	cliopatria:entailment/2.

cliopatria:entailment(rdfslite, rdfslite_entailment).
