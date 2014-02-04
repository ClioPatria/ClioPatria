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


:- module(skosxl_entailment,
	  [ rdf/3
	  ]).
:- use_module(rdfql(rdfql_runtime)).	% runtime tests
:- use_module(library(semweb/rdf_db),
	      [ rdf_global_id/2,
		rdf_subject/1,
		rdf_current_predicate/1,
		(rdf_meta)/1,
		op(_,_,_)
	      ]).

/** <module> SKOSXL entailment

The function of an entailment module is  to provide an implementation of
rdf/3 that extends basic triple-lookup using the entailment rules of the
SKOS-XL entailent rules

This entailment module does only some SKOS XL inferences, see the numbers of the rules.

*/

:- rdf_meta
	rdf(r,r,o).

rdf(S, P, O) :-
	rdf_db:rdf(S, P, O).

% S53
rdf(skosxl:prefLabel,   rdf:type, owl:'ObjectProperty').
rdf(skosxl:altLabel,    rdf:type, owl:'ObjectProperty').
rdf(skosxl:hiddenLabel, rdf:type, owl:'ObjectProperty').

% S54
rdf(skosxl:prefLabel,   rdfs:range,  skosxl:'Label').
rdf(skosxl:altLabel,    rdfs:range,  skosxl:'Label').
rdf(skosxl:hiddenLabel, rdfs:range,  skosxl:'Label').

% S55
rdf(S, skos:prefLabel, O) :-
	rdf_db:rdf(S, skosxl:prefLabel, L),
	rdf_db:rdf(L, skosxl:literalForm, O).
% S56
rdf(S, skos:altLabel, O) :-
	rdf_db:rdf(S, skosxl:altLabel, L),
	rdf_db:rdf(L, skosxl:literalForm, O).
% S57
rdf(S, skos:hiddenLabel, O) :-
	rdf_db:rdf(S, skosxl:hiddenLabel, L),
	rdf_db:rdf(L, skosxl:literalForm, O).



		 /*******************************
		 *	       REGISTER		*
		 *******************************/

:- multifile
	cliopatria:entailment/2.

cliopatria:entailment(skosxl, skosxl_entailment).
