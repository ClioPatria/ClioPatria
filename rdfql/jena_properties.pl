/*  Part of ClioPatria SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2012 VU University Amsterdam

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

:- module(jena_properties, []).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(aggregate)).
:- use_module(sparql_runtime).

:- multifile
	sparql:functional_property/2,
	sparql:current_functional_property/3.

ns(apf,   'http://jena.hpl.hp.com/ARQ/property#').
ns(lists, 'http://jena.hpl.hp.com/ARQ/list#').
ns(Prefix, URI) :-
	rdf_current_ns(Prefix, URI).

alias('java:com.hp.hpl.jena.sparql.pfunction.library.',
      'http://jena.hpl.hp.com/ARQ/property#').
alias('java:com.hp.hpl.jena.query.pfunction.library.',
      'http://jena.hpl.hp.com/ARQ/property#').

property_alias(Prefix:Local, Global) :-
	ns(Prefix, URI),
	alias(AliasBase, URI),
	atom_concat(AliasBase, Local, Global).

absolute_uri(Prefix:Local, Global) :-
	ns(Prefix, URI),
	atom_concat(URI, Local, Global).

term_expansion((sparql:functional_property(S, NS:Term0) :- Body),
	       [ (sparql:functional_property(S, Term) :- Body),
		 sparql:current_functional_property(P, P, Argc)
	       | Aliases
	       ]) :-
	Term0 =.. [Name|Args],
	length(Args, Argc),
	absolute_uri(NS:Name, P),
	Term =.. [P|Args],
	findall(sparql:current_functional_property(P1, P, Argc),
		property_alias(NS:Name, P1),
		Aliases).


		 /*******************************
		 *    JENA PROPERTY FUNCTIONS	*
		 *******************************/

% See http://jena.sourceforge.net/ARQ/library-propfunc.html

% (S apf:assign, O) is basically unification.

sparql:functional_property(S, apf:assign(O)) :-
	(   S = O
	->  true
	;   sparql_true(S=O)
	).


		 /*******************************
		 *	       LISTS		*
		 *******************************/

rdf_list(S) :-
	rdf_equal(S, rdf:nil).
rdf_list(S) :-
	rdf(S, rdf:first, _).

rdf_container(Container) :-
	container_class(Class),
	rdfs_individual_of(Container, Class).

:- rdf_meta container_class(r).

container_class(rdf:'Bag').
container_class(rdf:'Seq').
container_class(rdf:'Alt').

% (S, lists:member, O) means that O is a member of the collection S. In
% Jena, S may be unbound, finding all lists on the database.

sparql:functional_property(S, lists:member(O)) :-
	rdf_list(S),
	rdfs_member(O, S).

sparql:functional_property(S, rdfs:member(O)) :-
	rdf_container(S),
	rdfs_member(O, S).

sparql:functional_property(S, apf:bag(O)) :-
	nonvar(S),
	rdfs_individual_of(S, rdfs:'Bag'),
	rdfs_member(O, S).
sparql:functional_property(S, apf:seq(O)) :-
	nonvar(S),
	rdfs_individual_of(S, rdfs:'Seq'),
	rdfs_member(O, S).
sparql:functional_property(S, apf:alt(O)) :-
	nonvar(S),
	rdfs_individual_of(S, rdfs:'Alt'),
	rdfs_member(O, S).


% (S, lists:length, O) is true when O is the length of the collection S.
% Again, S may be unbound.

sparql:functional_property(S, lists:length(O)) :-
	rdf_list(S),
	aggregate_all(count, rdfs_member(_, S), Len),
	rdf_equal(xsd:integer, IntType),
	atom_number(String, Len),
	O = literal(type(IntType, String)).

sparql:functional_property(S, lists:index(literal(type(IntType, Index)),
					  Element)) :-
	rdf_list(S),
	rdf_equal(xsd:integer, IntType),
	(   var(Index)
	->  rdfs_nth1(I, S, Element),
	    atom_number(Index, I)
	;   atom_number(Index, I),
	    rdfs_nth1(I, S, Element)
	->  true
	).


rdfs_nth1(0, Set, Element) :-
	rdf_has(Set, rdf:first, Element).
rdfs_nth1(I, Set, Element) :-
	var(I), !,
	rdf_has(Set, rdf:rest, Tail),
	rdfs_nth1(I0, Tail, Element),
	I is I0 + 1.
rdfs_nth1(I, Set, Element) :-
	I2 is I - 1,
	rdf_has(Set, rdf:rest, Tail),
	rdfs_nth1(I2, Tail, Element).


