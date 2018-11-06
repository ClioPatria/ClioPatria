/*  Part of ClioPatria

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2018, VU University Amsterdam
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

:- module(flint, []).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/json)).
:- use_module(cliopatria(hooks)).

:- http_handler(flint('index.html'), sparql_editor, []).
:- http_handler(flint('config.js'), flint_config, []).
:- http_handler(flint(.), http_reply_from_files(flint(.), []), [prefix]).

:- html_resource(flint('config.js'),
                 [ requires([ flint('flint-editor.js'),

                              flint('css/sparqlcolors.css'),
                              flint('css/docs.css')
                            ])
                 ]).


:- html_resource(flint('lib/codemirror.js'),
                 [ requires([ flint('jquery-1.5.2.min.js'),
                              flint('lib/codemirror.css')
                            ])
                 ]).

:- html_resource(flint('flint-editor.js'),
                 [ requires([ flint('lib/codemirror.js'),
                              flint('sparql10querymode_ll1.js'),
                              flint('sparql11querymode_ll1.js'),
                              flint('sparql11updatemode_ll1.js')
                            ])
                 ]).

%!  sparql_editor(+Request)
%
%   HTTP handler that presents the flint SPARQL editor.

sparql_editor(_Request) :-
    \+ absolute_file_name(flint('flint-editor.js'), _,
                          [ access(read),
                            file_errors(fail)
                          ]),
    !,
    reply_html_page(cliopatria(default),
                    title('No Flint installed'),
                    \no_flint).
sparql_editor(_Request) :-
    reply_html_page(
        cliopatria(plain),
        title('Flint SPARQL Editor'),
        \flint_page).

flint_page -->
    html_requires(flint('config.js')),
    html(div(id(flint), [])).

%!  flint_config(+Request)
%
%   HTTP handler that serves the   flint SPARQL editor configuration
%   and initialization.

flint_config(_Request) :-
    config(Config),
    format('Content-type: text/javascript~n~n'),
    format('$(document).ready(function() {~n'),
    write_config(Config),
    write_init,
    format('});~n').

write_config(Config) :-
    format('  var flintConfig = '),
    json_write(current_output, Config, [width(72)]),
    format(';').

write_init :-
    format('  var fint = new FlintEditor("flint", "images", flintConfig);~n').

%!  config(-Config) is det.
%
%   Produce a JSON document holding the FlintEditor configuration.

config(json([ interface = json([ toolbar=true,
                                 menu=true
                               ]),
              namespaces = NameSpaces,
              defaultEndpointParameters = EndpointParameters,
              endpoints = EndPoints,
              defaultModes = Modes
            ])) :-
    namespaces(NameSpaces),
    endpoint_parameters(EndpointParameters),
    endpoints(EndPoints),
    modes(Modes).

namespaces(NameSpaces) :-
    setof(NameSpace, namespace(NameSpace), NameSpaces).

namespace(json([ name(Prefix),
                 prefix(Prefix),
                 uri(URI)
               ])) :-
    rdf_current_ns(Prefix, URI).

:- if(\+current_predicate(rdf_current_ns/2)).
rdf_current_ns(Prefix, URI) :- rdf_current_prefix(Prefix, URI).
:- endif.

endpoint_parameters(
    json([ queryParameters =
               json([ format(output),
                      query(query),
                      update(update)
                    ]),
           selectFormats =
             [ json([ name('Plain text'),
                      format(text),
                      type('text/plain')
                    ]),
               json([ name('SPARQL-XML'),
                      format(sparql),
                      type('application/sparql-results+xml')
                    ]),
               json([ name('JSON'),
                      format(json),
                      type('application/sparql-results+json')
                    ])
             ],
           constructFormats =
             [ json([ name('Plain text'),
                      format(text),
                      type('text/plain')
                    ]),
               json([ name('RDF/XML'),
                      format(rdfxml),
                      type('application/rdf+xml')
                    ]),
               json([ name('Turtle'),
                      format(turtle),
                      type('application/turtle')
                    ])
             ]
         ])).


endpoints([ json([ name('ClioPatria'),
                   uri(EndPoint),
                   modes([ sparql11query, sparql10 ])
                 ]),
            json([ name('Update'),
                   uri(UpdateEndPoint),
                   modes([ sparql11update ])
                 ])
          ]) :-
    http_link_to_id(sparql_query,  [], EndPoint),
    http_link_to_id(sparql_update, [], UpdateEndPoint).


modes([ json([ name('SPARQL 1.1 Query'),
               mode(sparql11query)
             ]),
        json([ name('SPARQL 1.1 Update'),
               mode(sparql11update)
             ]),
        json([ name('SPARQL 1.0'),
               mode(sparql10)
             ])
      ]).


%!  no_flint//
%
%   Display a message indicating the user how to install Flint

no_flint -->
    { absolute_file_name(cliopatria(.), CD0,
                         [ file_type(directory),
                           access(read)
                         ]),
      prolog_to_os_filename(CD0, ClioHome)
    },
    html_requires(pldoc),
    html([ h1('Flint SPARQL Editor is not installed'),
           p([ 'Please run the following command in the ClioPatria ',
               'installation directory "~w" to install Flint.'-[ClioHome]
             ]),
           pre(class(code),
               [ 'git submodule update --init web/FlintSparqlEditor'
               ])
         ]).


                 /*******************************
                 *       REGISTER WITH MENU     *
                 *******************************/

cliopatria:menu_item(150=query/sparql_editor,  'Flint SPARQL Editor').
