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


:- module(rdfs_entailment,
	  [
	  ]).
:- use_module(rdfql(rdfql_runtime)).	% runtime tests
:- use_module(library(nb_set)).
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
		rdfs_subproperty_of/2
	      ]).

/** <module> RDFS entailment

The function of an entailment module is  to provide an implementation of
rdf/3 that extends basic triple-lookup using the entailment rules of the
semantic web sub language of RDF.

This entailment module does RDFS entailment.

@tbd	Check the completeness
*/

:- rdf_meta
	rdf(o,o,o),
	individual_of(r,r).

:- public
	rdf/3.

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
rdf(S, P, C) :-
	rdf_reachable(rdf:type, rdfs:subPropertyOf, P), !,
	individual_of(S, C).
rdf(S, P, O) :-					% transitive predicates
	rdf_reachable(rdfs:subClassOf, rdfs:subPropertyOf, P), !,
	(   (nonvar(S) ; nonvar(O))
	->  rdfs_subclass_of(S, O)		% generate from given start
	;   individual_of(S, rdfs:'Class'),	% generated unbounded (slow!)
	    rdfs_subclass_of(S, O)
	).
rdf(S, rdfs:subPropertyOf, O) :- !,
	(   nonvar(S)
	->  individual_of(S, rdf:'Property'),
	    rdfs_subproperty_of(S, O)
	;   nonvar(O)
	->  individual_of(O, rdf:'Property'),
	    rdfs_subproperty_of(S, O)
	;   individual_of(S, rdf:'Property'),
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


%%	individual_of(?Resource, ?Class)

individual_of(Resource, Class) :-
	nonvar(Resource), !,
	(   Resource = literal(_)
	->  rdfs_subclass_of(Class, rdfs:'Literal')
	;   rdfs_has_type(Resource, MyClass),
	    rdfs_subclass_of(MyClass, Class)
	;   rdf_equal(Class, rdfs:'Resource')
	).
individual_of(Resource, Class) :-
	nonvar(Class), !,
	(   rdf_equal(Class, rdfs:'Resource')
	->  rdf_subject(Resource)
	;   rdfs_subclass_of(SubClass, Class),
	    rdfs_has_type(Resource, SubClass)
	).
individual_of(Resource, Class) :-
	rdf_subject(Resource),
	individual_of(Resource, Class).


%%	rdfs_has_type(+Resource, -Class) is nondet.
%%	rdfs_has_type(-Resource, +Class) is nondet.
%
%	Perform RDFS entailment rules to enumerate the types of Resource
%	or generate all resources entailed  by   the  given  class.

rdfs_has_type(Resource, Class) :-
	empty_nb_set(Set),
	(   atom(Resource)
	->  (   rdf_has(Resource, rdf:type, Class)
	    ;	rdf_db:rdf(Resource, P, _),
		rdf_has(P, rdfs:domain, Class)
	    ;	rdf_db:rdf(_, P, Resource),
		rdf_has(P, rdfs:range, Class)
	    ;	rdf_db:rdf(Resource, rdfs:subPropertyOf, _),
		rdf_equal(Class, rdf:'Property')
	    ;	rdf_db:rdf(_, rdfs:subPropertyOf, Resource),
		rdf_equal(Class, rdf:'Property')
	    ;	(   rdf_db:rdf(_,Resource,_)
		->  rdf_equal(Class, rdf:'Property'),
		    \+ rdf_has(Resource, rdf:type, Class)
		)
	    ),
	    add_nb_set(Class, Set, New),
	    New == true
	;   atom(Class)
	->  (	rdf_has(Resource, rdf:type, Class)
	    ;	rdf_has(P, rdfs:domain, Class),
		rdf_has(Resource, P, _)
	    ;	rdf_has(P, rdfs:range, Class),
		rdf_has(_, P, Resource)
	    ;	(   rdf_reachable(Class, rdfs:subClassOf, rdf:'Property')
		->  rdf_current_predicate(Resource),
		    (   rdf(_,Resource,_)
		    ->  \+ rdf_has(Resource, rdf:type, Class)
		    )
		)
	    ),
	    add_nb_set(Resource, Set, New),
	    New == true
	;   throw(error(instantiation_error, _))
	).


		 /*******************************
		 *	       REGISTER		*
		 *******************************/

:- multifile
	cliopatria:entailment/2.

cliopatria:entailment(rdfs, rdfs_entailment).
