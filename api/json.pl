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

:- module(api_json,
	  [
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_json)).
:- use_module(library(semweb/rdf_describe)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

:- http_handler(json(describe), json_describe, []).

/* <module> Describe resources in JSON

This module produces a JSON description for a resource.

@see	sparql.pl implements a SPARQL frontend.  The SPARQL DESCRIBE
	keyword provides similar functionality, but it has no arguments
	to specify how to describe a resource and it cannot return JSON.
@see	lod.pl implements Linked Open Data (LOD) descriptions.
*/

%%	json_describe(+Request)
%
%	HTTP  handler  that  describes   a    resource   using   a  JSON
%	serialization.
%
%	@see	http://n2.talis.com/wiki/RDF_JSON_Specification describes
%		the used graph-serialization.
%	@see	http://n2.talis.com/wiki/Bounded_Descriptions_in_RDF for
%		a description of the various descriptions
%	@bug	Currently only supports =cbd=.

json_describe(Request) :-
	http_parameters(Request,
			[ r(URI,
			    [ description('The resource to describe')
			    ]),
			  how(_How,
			      [ %oneof([cbd, scdb, ifcbd, lcbd, hcbd]),
				oneof([cbd]),
				default(cbd),
				description('Algorithm that determines \
					     the description')
			      ])
			]),
	resource_CBD(rdf, URI, Graph),
	graph_json(Graph, JSON),
	reply_json(JSON).


