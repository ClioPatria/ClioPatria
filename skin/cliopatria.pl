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

:- module(cp_skin,
          [ server_address//1,          % +Component
            current_page_doc_link//0
          ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(version)).
:- use_module(components(menu)).
:- use_module(components(simple_search)).
:- use_module(applications(help/version)).

/** <module> ClioPatria skin

This page defines the overall layout of  ClioPatria pages. All pages are
returned using reply_html_page/3, using the   page class cliopatria(Id),
where Id is currently  always  =default=.   Pages  can  be  redefined by
providing a rule for user:body//2, where   the first argument must unify
with the page class.

The default skin provides the overall menu,   a  simple search form, the
content and the `server-address'. Because the   search-form uses the YUI
autocomplete widgets, the body must include class =|yui-skin-sam|=.  The
default body has the classes =|yui-skin-sam|= and =cliopatria=.

The default skin provided by this can be overruled using two hooks:

        $ cliopatria:page_body//1 :
        Emit a page from the given content.  This hook can be used to modify
        the overall page layout beyond what can be achieved with CSS.
        $ cliopatria:server_address//0 :
        Write the address of the server.

This   library   also   provides   building     blocks,    notably   for
server_address//0:

        $ server_address//1 :
        Presents the version info and a link to a GIT module.
        $ current_page_doc_link//0 :
        Presents a link to the documentation of a page if the
        self-documentation facilities are loaded.  See run.pl.in.

The CSS file css('cliopatria.css') contains the ClioPatria style that is
makes ClioPatria look pretty to our  eyes,   but  is  not essential. The
plugin config-available/fix_menu.pl contains example code  to extend the
ClioPatria skin.
*/

:- http_handler('/favicon.ico',
                http_reply_file(icons('favicon.ico'), []),
                []).

:- html_resource(js('cliopatria.js'),
                 [ requires([jquery])
                 ]).
:- html_resource(plain,
                 [ virtual(true),
                   requires([ css('plain.css')
                            ])
                 ]).
:- html_resource(cliopatria,
                 [ virtual(true),
                   requires([ css('cliopatria.css'),
                              js('cliopatria.js')
                            ])
                 ]).

%!  user:body(+Style, :Body)// is det.
%
%   The multi-file implementation defines the overall layout of HTML
%   pages with the Style cliopatria(_).

:- multifile
    user:body//2.

user:body(cliopatria(Style), Body) -->
    cliopatria:page_body(cliopatria(Style), Body),
    !.
user:body(cliopatria(_), Body) -->
    cliopatria:page_body(Body),
    !.
user:body(cliopatria(plain), Body) -->
    html_requires(plain),
    html(body(class(['yui-skin-sam', cliopatria]),
              [ div([id('cp-menu'), class(menu)], \cp_logo_and_menu),
                \simple_search_form([value(p(q))]),
                br(clear(all)),
                div([id('cp-content'), class(content)], Body),
                br(clear(all)),
                div([id('cp-footer'), class(footer)], \address)
              ])).
user:body(cliopatria(_), Body) -->
    html_requires(cliopatria),
    html(body(class(['yui-skin-sam', cliopatria]),
              [ div([id('cp-menu'), class(menu)], \cp_logo_and_menu),
                \simple_search_form([value(p(q))]),
                br(clear(all)),
                div([id('cp-content'), class(content)], Body),
                br(clear(all)),
                div([id('cp-footer'), class(footer)], \address)
              ])).

cp_logo_and_menu -->
    cp_logo,
    cp_menu.

cp_logo -->
    cliopatria:logo,
    !.
cp_logo -->
    { File = 'cliopatria-logo.png',
      absolute_file_name(icons(File), _Logo,
                         [access(read), file_errors(fail)]),
      http_absolute_location(icons(File), Src, []),
      http_link_to_id(home, [], Home)
    },
    html(a([class(logo), href(Home), style('float:left')
           ],
           img([src(Src)]))).

%!  address//
%
%   Emit an element =address= with   class  =cliopatria=. This first
%   class  the  hook  cliopatria:server_address//0.  If  this  hooks
%   fails, it calls server_address('ClioPatria').
%
%   @see version.pl

address -->
    cliopatria:server_address,
    !.
address -->
    server_address('ClioPatria').


%!  server_address(+Component)//
%
%   HTML component that emits the   default ClioPatria address link.
%   This provides a link to the ClioPatria   home page and the (GIT)
%   version information. ClioPatria  is  registered   with  the  GIT
%   module =|ClioPatria|= and the default server address is provided
%   by calling:
%
%       ==
%           ...,
%           server_address('ClioPatria'),
%           ...
%       ==
%
%   @see register_git_module/2 for registering a GIT module.

server_address(Component) -->
    html([ address(class(footer),
                   [ \component_address(Component),
                     \current_page_doc_link
                   ])
         ]).

%!  component_address(+Name)//
%
%   The label ClioPatria as a link to its home-page on the web.

component_address(Component) -->
    (   { git_module_property(Component, home_url(Home)) }
    ->  html(a([ class(home), href(Home),
                 title(Component+' home')
               ], Component))
    ;   html(span(class(home), Component))
    ),
    html(' (version '),
    component_version(Component),
    html(')').


%!  component_version(+Name)//
%
%   Give verion information and link to detailed version info

component_version(Component) -->
    { (   git_module_property(Component, version(Version))
      ->  true
      ;   Version = 'no GIT?'
      ),
      http_link_to_id(version_info, [], VREF)
    },
    html(a([title('About versions'),
            class(version),
            href(VREF)],
           Version)).



%!  current_page_doc_link//
%
%   Create a link to  the  documentation   (and  from  there  to the
%   implementation) of this page. This link   is created only if the
%   library applications(help/http_help) is loaded.

:- if(current_predicate(http_help:page_documentation_link//1)).
current_page_doc_link -->
    { http_current_request(Request) },
    http_help:page_documentation_link(Request).
:- else.
current_page_doc_link --> [].
:- endif.
