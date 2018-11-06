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

:- module(api_rdflib,
          [ library_ontology/1          % -Name
          ]).
:- use_module(user(user_db)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_library)).
:- use_module(rdfql(rdf_io)).
:- use_module(sesame).
:- use_module(components(messages)).

/** <module> Provide access to the ontology library

@see library(semweb/rdf_library)
*/

:- http_handler(sesame('loadLibraryOntology'),   load_library_ontology,
                [time_limit(infinite)]).
:- http_handler(sesame('listLibraryOntologies'), list_library_ontologies, []).

%!  load_library_ontology(+Request)
%
%   Load a named ontology from the ontology library.
%
%   @tbd    Cannot use concurrent loading as the load as a whole is
%           inside an rdf transaction.

load_library_ontology(Request) :-
    http_parameters(Request,
                    [ repository(Repository),
                      ontology(Ontology, []),
                      resultFormat(Format)
                    ],
                    [ attribute_declarations(attribute_decl)
                    ]),
    authorized(write(Repository, load(library_ontology(Ontology)))),
    prepare_ontology_library,
    api_action(Request,
               rdf_load_library(Ontology, []),
               Format,
               \loaded_library_ontology(Ontology)).

loaded_library_ontology(Id) -->
    html('Loaded RDF library '),
    (   { rdf_library_index(Id, title(Title)) }
    ->  html([Id, ' -- ', Title])
    ;   html(Id)
    ).


%!  list_library_ontologies(+Request)
%
%   Reply with a list of available base ontologies

list_library_ontologies(Request) :-
    authorized(read(status, listBaseOntologies)),
    http_parameters(Request,
                    [ resultFormat(Format),
                      serialization(Serialization)
                    ],
                    [ attribute_declarations(attribute_decl)
                    ]),
    catch(findall(row(O), library_ontology(O), Rows0), _,
          Rows0 = []),
    sort(Rows0, Rows),
    write_table(Rows,
                [ result_format(Format),
                  serialization(Serialization),
                  variables(varnames(ontology))
                ]).


%!  library_ontology(-Name) is nondet.
%
%   True if Name is the name of an ontology from the library.

library_ontology(O) :-
    prepare_ontology_library,
    rdf_library_index(O, title(_Title)).


%!  prepare_ontology_library is det.
%
%   Load RDF library manifests from   directories  defined using the
%   file_search_path/2 =ontology= alias.

prepare_ontology_library :-
    (   absolute_file_name(rdf(.), Dir,
                           [ file_type(directory),
                             solutions(all)
                           ]),
        rdf_attach_library(Dir),
        fail
    ;   true
    ).

%!  attribute_decl(+Name, -Options)
%
%   Copied from Sesame

attribute_decl(repository,
               [ optional(true),
                 description('Name of the repository (ignored)')
               ]).
attribute_decl(resultFormat,
               [ default(xml),
                 type(oneof([ xml,
                              html,
                              rdf
                            ])),
                 description('Serialization format of the result')
               ]).
attribute_decl(ontology,
               [ description('Name of the ontology to load')
               ]).
