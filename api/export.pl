/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, VU University Amsterdam
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

:- module(api_export, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_schema)).
:- use_module(rdfql(rdf_io)).
:- use_module(rdfql(rdf_turtle_io)).
:- use_module(user(user_db)).

:- http_handler(api(export_graph),         export_graph,  []).
:- http_handler(api(export_graph_schema),  export_graph_schema,  []).

/** <module> Export data from the server

*/

%!  export_graph(+Request)
%
%   Export a named graph in a   given  serialization. Whether or not
%   exporting of a named graph is  defined by authorized/1 using the
%   term:
%
%           * read(default, download(Graph))

export_graph(Request) :-
    http_parameters(Request,
                    [ graph(Graph),
                      format(Format),
                      mimetype(Mime)
                    ],
                    [ attribute_declarations(http_param)
                    ]
                   ),
    authorized(read(default, download(Graph))),
    send_graph(Graph, Format, Mime).

send_graph(Graph, Format, default) :-
    !,
    default_mime_type(Format, MimeType),
    send_graph(Graph, Format, MimeType).
send_graph(Graph, Format, MimeType) :-
    !,
    format('Transfer-Encoding: chunked~n'),
    format('Content-type: ~w; charset=UTF8~n~n', [MimeType]),
    send_graph(Graph, Format).

send_graph(Graph, turtle) :-
    !,
    rdf_save_turtle(stream(current_output),
                    [ graph(Graph),
                      base(Graph)
                    ]).
send_graph(Graph, canonical_turtle) :-
    !,
    rdf_save_canonical_turtle(stream(current_output), [graph(Graph)]).
send_graph(Graph, rdfxml) :-
    !,
    rdf_save(stream(current_output), [graph(Graph)]).

default_mime_type(turtle, text/turtle).
default_mime_type(canonical_turtle, text/turtle).
default_mime_type(rdfxml, application/'rdf+xml').

%!  export_graph_schema(+Request)
%
%   HTTP handler that computes the schema from the actual data in a
%   graph.
%
%   @see The computation is implemented by rdf_graph_schema/2.

export_graph_schema(Request) :-
    http_parameters(Request,
                    [ graph(Graph),
                      format(Format),
                      mimetype(Mime)
                    ],
                    [ attribute_declarations(http_param)
                    ]
                   ),
    authorized(read(default, download(Graph))),
    rdf_graph_schema(Graph, Triples),
    (   Mime == default
    ->  default_mime_type(Format, MimeType)
    ;   MimeType = Mime
    ),
    write_graph(Triples,
                [ serialization(Format),
                  mimetype(MimeType)
                ]).


%!  http_param(?Name, ?Attributes).

http_param(graph,
           [ description('Name of the graph')]).
http_param(format,
           [ oneof([turtle,
                    canonical_turtle,
                    rdfxml
                   ]),
             default(turtle),
             description('Output serialization')
           ]).
http_param(mimetype,
           [ default(default),
             description('MIME-type to use. If "default", it depends on format')
           ]).



