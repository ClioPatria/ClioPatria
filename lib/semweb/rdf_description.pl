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

:- module(rdf_description,
	  [ description_property/1,	% ?Property
	    rdf_description/2		% +Resource, ?Description
	  ]).
:- use_module(library(semweb/rdf_db)).

/** <module> Deal with descriptive nodes in RDF models

@tbd	Define sensible operations on description properties, similar
	to rdf_label.pl.
@tbd	Language selection
*/

:- rdf_meta
	description_property(r).
:- multifile
	description_property/1,
	description_hook/2.

%%	description_property(?Property)
%
%	True when Property is used for   descritive nodes on a resource.
%	This predicate is multifile.

description_property(dc:description).
description_property(skos:scopeNote).
description_property(rdfs:comment).


%%	rdf_description(+R, -Description:literal) is nondet.
%
%	Description is a description for R.   This predicate first calls
%	the hook description_hook/2. If this hook  fails it produces all
%	property-values    for    the     properties      defined     by
%	description_property/1 that have a literal value.

rdf_description(R, Description) :-
	(   description_hook(R, Description)
	*-> true
	;   description_property(P),
	    rdf_has(R, P, Description),
	    rdf_is_literal(Description)
	).
