/*  Part of ClioPatria

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2012 University of Amsterdam
                             CWI, Asterdam
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

:- module(cp_help, []).
:- use_module(library(doc_http)).               % Load pldoc
:- use_module(library(pldoc/doc_index)).        % PlDoc Search menu
:- use_module(library(http/http_hook)).         % Get hook signatures
:- use_module(library(http/http_dispatch)).     % Get hook signatures
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- include(library(pldoc/hooks)).

:- use_module(cliopatria(parms)).       % Get paths
:- use_module(skin(cliopatria)).        % Skinning primitives
:- use_module(wiki).                    % Our own help-pages
:- use_module(http_help).               % Help on HTTP server
:- use_module(ac_predicate).            % Predicate autocompletion
:- use_module(components(menu)).        % ClioPatria Menu

/** <module> ClioPatria help system

This   module   serves   the   wiki-source     based   help-pages   from
cliopatria(web/help)  and  integrates   SWI-Prolog's    PlDoc   literate
programming system to provide documentation of the source-code.
*/

:- if(current_predicate(doc_enable/1)).
:- initialization
    doc_enable(true).
:- endif.

%       http:location(pldoc, Location, Options) is det.
%
%       Rebase PlDoc to <prefix>/help/source/

http:location(pldoc, root('help/source'), [priority(10)]).

:- http_handler(root(help/source), cp_help, []).
:- http_handler(cliopatria('help/'),
                serve_page(help), [prefix, id(wiki_help)]).
:- http_handler(cliopatria('tutorial/'),
                serve_page(tutorial), [prefix, id(tutorial)]).

%!  prolog:doc_directory(+Dir) is semidet.
%
%   True if we allow PlDoc to  serve   files  from  Dir. This allows
%   serving all files in the ClioPatria hierarchy.

prolog:doc_directory(Dir) :-
    absolute_file_name(cliopatria(.), CpDir,
                       [ file_type(directory),
                         access(read)
                       ]),
    sub_atom(Dir, 0, _, _, CpDir).

%!  cp_help(+Request)
%
%   HTTP handler that integrates a customised   version of PlDoc for
%   ClioPatria.  The opening page shows the file RoadMap.txt.

cp_help(Request) :-
    http_location_by_id(pldoc_doc, Location),
    absolute_file_name(cliopatria('RoadMap'), HelpFile,
                       [ extensions([txt]),
                         access(read)
                       ]),
    atom_concat(Location, HelpFile, StartPage),
    http_redirect(moved, StartPage, Request).

%!  cliopatria:menu_item(-Item, -Label) is nondet.
%
%   Extends the help popup with  links   to  the source-code and the
%   HTTP services.

:- multifile
    cliopatria:menu_item/2.

cliopatria:menu_item(100=help/wiki_help, 'Documentation').
cliopatria:menu_item(150=help/tutorial,  'Tutorial').
cliopatria:menu_item(200=help/cp_help,   'Roadmap').
cliopatria:menu_item(300=help/http_help, 'HTTP Services').

%!  user:body(+Style, :Body)// is det.
%
%   The multi-file implementation defines the overall layout of HTML
%   pages with the Style pldoc(_).

:- multifile
    user:body//2.

user:body(pldoc(wiki), Content) -->
    { absolute_file_name(cliopatria(.), Dir,
                         [ file_type(directory),
                           access(read)
                         ])
    },
    html_requires(cliopatria),
    html(body(class('yui-skin-sam cliopatria'),
              [ div(class(menu), \cp_menu),
                br(clear(all)),
                div(class(content),
                    [ \doc_links(Dir, [])
                    | Content
                    ]),
                \server_address('ClioPatria')
              ])).
user:body(pldoc(_), Content) -->
    html_requires(cliopatria),
    html(body(class('yui-skin-sam cliopatria'),
              [ div(class(menu), \cp_menu),
                br(clear(all)),
                div(class(content), Content),
                \server_address('ClioPatria')
              ])).
