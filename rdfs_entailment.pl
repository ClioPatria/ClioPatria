/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2004, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(rdfs_entailment,
	  [ 
	  ]).
:- use_module(rdfql_runtime).			% runtime tests
:- use_module(library(nb_set)).
:- use_module(library('semweb/rdf_db'),
	      [ rdf_global_id/2,
		rdf_reachable/3,
		rdf_has/3,
		rdf_subject/1,
		rdf_equal/2
	      ]).
:- use_module(library('semweb/rdfs'),
	      [ rdfs_subclass_of/2,
		rdfs_subproperty_of/2
	      ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The function of an entailment module is  to provide an implementation of
rdf/3 that extends basic triple-lookup using the entailment rules of the
semantic web sub language of RDF.  This   one  does  (still a lousy) job
realising RDFS entailment on top of rdf_db.pl.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

term_expansion((rdf(S0, P0, O0) :- Body),
	       (rdf(S,  P,  O)  :- Body)) :-
	rdf_global_id(S0, S),
	rdf_global_id(P0, P),
	rdf_global_id(O0, O).

rdf(literal(L), _, _) :-		% should move to compiler
	nonvar(L), !, fail.
rdf(_, literal(L), _) :-		% idem
	nonvar(L), !, fail.
rdf(S, P, O) :-
	var(P), !,
	(   rdf_db:rdf(S,P,O)
	;   rdf(P, rdf:type, rdf:'Property'),
	    rdf(S, P, O),
	    \+ rdf_db:rdf(S,P,O)
	).
rdf(S, rdf:type, C) :- !,
	(   nonvar(C)
	->  rdfs_individual_of(S, C)
	;   rdf_subject(S),			% ensure instantiation
	    rdfs_individual_of(S, C)
	).
rdf(S, rdfs:subClassOf, O) :- !,		% transitive predicates
	(   (nonvar(S) ; nonvar(O)),
	    S \== O				% avoid X rdfs:subClassOf X
	->  rdfs_subclass_of(S, O)
	;   rdfs_individual_of(S, rdfs:'Class'),
	    rdfs_subclass_of(S, O)
	).
rdf(S, rdfs:subPropertyOf, O) :- !,
	(   nonvar(S)
	->  rdfs_individual_of(S, rdf:'Property'),
	    rdfs_subproperty_of(S, O)
	;   nonvar(O)
	->  rdfs_individual_of(O, rdf:'Property'),
	    rdfs_subproperty_of(S, O)
	;   rdfs_individual_of(S, rdf:'Property'),
	    rdfs_subproperty_of(S, O)
	).
rdf(S, serql:directSubClassOf, O) :- !,
	rdf_has(S, rdfs:subClassOf, O).
rdf(S, serql:directType, O) :- !,
	rdf_has(S, rdf:type, O).
rdf(S, serql:directSubPropertyOf, O) :- !,
	rdf_has(S, rdfs:subPropertyOf, O).
rdf(S, P, O) :-
	rdf_has(S, P, O).


%%	rdfs_individual_of(?Resource, ?Class)

rdfs_individual_of(Resource, Class) :-
	nonvar(Resource), !,
	(   Resource = literal(_)
	->  rdfs_subclass_of(Class, rdfs:'Literal')
	;   rdfs_has_type(Resource, MyClass),
	    rdfs_subclass_of(MyClass, Class)
	;   rdf_equal(Class, rdfs:'Resource')
	).
rdfs_individual_of(Resource, Class) :-
	nonvar(Class), !,
	(   rdf_equal(Class, rdfs:'Resource')
	->  rdf_subject(Resource)
	;   rdfs_subclass_of(SubClass, Class),
	    rdfs_has_type(Resource, SubClass)
	).
rdfs_individual_of(Resource, Class) :-
	rdf_subject(Resource),
	rdfs_individual_of(Resource, Class).


%%	rdfs_has_type(+Resource, -Class) is nondet.
%%	rdfs_has_type(-Resource, +Class) is nondet.
%	
%	Perform RDFS entailment rules to enumerate the types of Resource
%	or generate all resources entailed  by   the  given  class.

rdfs_has_type(Resource, Class) :-
	empty_nb_set(Set),
	(   atom(Resource)
	->  (   rdf_has(Resource, rdf:type, Class)
	    ;	rdf_has(Resource, P, _),
		rdf_has(P, rdfs:domain, Class)
	    ;	rdf_has(_, P, Resource),
		rdf_has(P, rdfs:range, Class)
	    ),
	    add_nb_set(Class, Set, New),
	    New == true
	;   atom(Class)
	->  (	rdf_has(Resource, rdf:type, Class)
	    ;	rdf_has(P, rdfs:domain, Class),
		rdf_has(Resource, P, _)
	    ;	rdf_has(P, rdfs:range, Class),
		rdf_has(_, P, Resource)
	    ),
	    add_nb_set(Resource, Set, New),
	    New == true
	;   throw(error(instantiation_error, _))
	).
	       

		 /*******************************
		 *	       REGISTER		*
		 *******************************/

:- multifile
	serql:entailment/2.

serql:entailment(rdfs, rdfs_entailment).
