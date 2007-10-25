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

:- module(rdf_entailment,
	  [ rdf/3
	  ]).
:- use_module(rdfql_runtime).			% runtime tests
:- use_module(library('semweb/rdf_db'),
	      [ rdf_global_id/2,
		rdf_subject/1
	      ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The function of an entailment module is  to provide an implementation of
rdf/3 that extends basic triple-lookup using the entailment rules of the
semantic web sub language of RDF.  This   one  does  (still a lousy) job
realising RDFS entailment on top of rdf_db.pl.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

term_expansion((rdf(S0, P0, O0) :- Body0),
	       (rdf(S,  P,  O)  :- rdf_db:Body)) :-
	rdf_global_id(S0, S),
	rdf_global_id(P0, P),
	rdf_global_id(O0, O),
	expand_goal(Body0, Body).

rdf(S, P, O) :-
	rdf(S, P, O).
rdf(S, rdf:type, rdf:'Property') :-
	rdf(_, S, _),
	\+ rdf(S, rdf:type, rdf:'Property').
rdf(S, rdf:type, rdfs:'Resource') :-
	rdf_subject(S),
	\+ rdf(S, rdf:type, rdfs:'Resource').
rdf(S, serql:directSubClassOf, O) :- !,
	rdf(S, rdfs:subClassOf, O).
rdf(S, serql:directType, O) :- !,
	rdf(S, rdf:type, O).
rdf(S, serql:directSubPropertyOf, O) :- !,
	rdf(S, rdfs:subPropertyOf, O).


		 /*******************************
		 *	       REGISTER		*
		 *******************************/

:- multifile
	serql:entailment/2.

serql:entailment(rdf, rdf_entailment).
