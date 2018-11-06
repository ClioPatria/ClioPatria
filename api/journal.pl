/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam
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

:- module(api_journal, []).
:- use_module(library(semweb/rdf_persistency)).
:- use_module(user(user_db)).
:- use_module(library('http/http_parameters')).
:- use_module(library('http/mimetype')).
:- use_module(library('http/http_dispatch')).

/** <module> RDF DB journal-API

This module exports the journal files   defined in rdf_persistency. This
is will be used to synchronise servers.
*/

:- http_handler(cliopatria(list_journals), list_journals, []).
:- http_handler(cliopatria(journal),       journal,       []).

%!  list_journals(+Request)
%
%   HTTP handler to list the available journals from the persistent
%   store as an XML document.  Here is an example document:
%
%   ==
%   <journals>
%     <journal db="http://www.example.org/db1"/>
%     <journal db="http://www.example.org/db2"/>
%     ...
%   </journals>
%   ==
%
%   @see /journal

list_journals(_Request) :-
    !,
    authorized(read(default, list_journals)),
    format('Content-type: text/xml~n~n'),
    format('<journals>~n'),
    forall(rdf_journal_file(DB, _File),
           format('  <journal db="~w"/>~n', [DB])),
    format('</journals>~n').

%!  journal(+Request)
%
%   HTTP handler that serves the journal for an RDF named graph as a
%   Prolog text. If no journal  is   available  for  the given named
%   graph, it returns a 404 page.
%
%   @see /list_journals


journal(Request) :-
    !,
    http_parameters(Request,
                    [ 'DB'(DB, [description('URI for the named graph')])
                    ],
                    [
                        ]),
    authorized(read(DB, read_journal)),
    (   rdf_journal_file(DB, Path)
    ->  http_reply_file(Path, [mime_type(text/'x-prolog')], Request)
    ;   existence_error(http_location, DB)
    ).
