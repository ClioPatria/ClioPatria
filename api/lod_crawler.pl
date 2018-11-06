/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam,
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
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

%!  lod_crawl(+Request)
%
%   HTTP handler requesting ClioPatria to crawl LOD.

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
    var(Return),
    !.
return_option(Return, [ return_to(Return) ]).


%!  lod_uri_graph(+URI, -Graph)
%
%   Determine the graph in which to dump   LOD from URI. This simply
%   deletes a possible fragment (#...) from the URI.

lod_uri_graph(URI, Graph) :-
    uri_components(URI, Components),
    uri_data(fragment, Components, Fragment),
    nonvar(Fragment),
    !,
    uri_data(fragment, Components, _, NewComponents),
    uri_components(Graph, NewComponents).
lod_uri_graph(URI, URI).

:- multifile
    rdf_http_plugin:rdf_content_type/2.

rdf_http_plugin:rdf_content_type('text/xml', xml).
