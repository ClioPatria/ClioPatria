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

:- module(api_lod_crawler,
	  [ lod_uri_graph/2
	  ]).
:- use_module(library(uri)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(components(messages)).
:- use_module(user(user_db)).

:- http_handler(api(lod_crawl), lod_crawl, []).

%%	lod_crawl(+Request)
%
%	HTTP handler requesting ClioPatria to crawl LOD.

lod_crawl(Request) :-
	authorized(write(default, load(lod))),
	http_parameters(Request,
			[ r(URI,
			    [ description('URI to start')
			    ]),
			  return_to(Return,
				    [ optional(true),
				      description('URI to return to')
				    ])
			]),
	lod_uri_graph(URI, Graph),
	return_option(Return, Options),
	call_showing_messages(rdf_load(Graph,
				       [ graph(Graph)
				       ]),
			      Options).

return_option(Return, []) :-
	var(Return), !.
return_option(Return, [ return_to(Return) ]).


%%	lod_uri_graph(+URI, -Graph)
%
%	Determine the graph in which to dump   LOD from URI. This simply
%	deletes a possible fragment (#...) from the URI.

lod_uri_graph(URI, Graph) :-
	uri_components(URI, Components),
	uri_data(fragment, Components, Fragment),
	nonvar(Fragment), !,
	uri_data(fragment, Components, _, NewComponents),
	uri_components(Graph, NewComponents).
lod_uri_graph(URI, URI).

:- multifile
	rdf_http_plugin:rdf_content_type/2.

rdf_http_plugin:rdf_content_type('text/xml', xml).
