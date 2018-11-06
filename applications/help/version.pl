/*  Part of ClioPatria

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2011 University of Amsterdam
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

:- module(cpa_version_help,
          [
          ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(version)).
:- use_module(library(occurs)).
:- use_module(library(sgml)).
:- use_module(library(lists)).
:- use_module(components(basics)).

/** <module> Provide detailed version information
*/

:- http_handler(root(help/versions), version_info, []).

%!  version_info(+Request)
%
%   HTTP handler that provides detailed   information  on the loaded
%   (and registered) GIT modules.

version_info(_Request) :-
    reply_html_page(cliopatria(default),
                    [ title('Version details') ],
                    [ h4('GIT modules'),
                      \git_modules,
                      h4('Server implementation language'),
                      p(\prolog_version),
                      div(class(textbox), \about_git_versions)
                    ]).

%!  git_modules//
%
%   Component that creates a table of registered GIT modules.
%
%   @see register_git_module/2

git_modules -->
    { findall(C-V, git_module_property(C, version(V)), Pairs) },
    html(table(class(block),
               [ tr([ th('GIT module'), th('Version'), th('Directory') ]),
                 \git_modules(Pairs)
               ])).

git_modules([]) --> [].
git_modules([H|T]) -->
    git_module(H),
    git_modules(T).

git_module(Name-Version) -->
    { git_module_property(Name, directory(Dir)) -> true },
    html(tr([td(\home_link(Name)), td(Version), td(Dir)])).

home_link(Component) -->
    { git_module_property(Component, home_url(Home)) },
    !,
    html(a(href(Home), Component)).
home_link(Component) -->
    html(Component).


%!  prolog_version//
%
%   Component that emits the current version of SWI-Prolog

prolog_version -->
    { current_prolog_flag(version_data, swi(Major, Minor, Patch, _)),
      atomic_list_concat([Major, Minor, Patch], '.', Version)
    },
    html([ a(href('http://www.swi-prolog.org'), 'SWI-Prolog'), ' ',
           'version ', b(Version)
         ]),
    (   { current_prolog_flag(version_git, GitVersion),
          GitVersion \== Version
        }
    ->  html([' (GIT version ', b(GitVersion), ')'])
    ;   []
    ).

%!  about_git_versions//
%
%   Component    that    emits    the    both      of    the    file
%   html('git-versions.html'), explaining the background  behind git
%   stamped versions.


about_git_versions -->
    insert_html_file(html('git-versions.html')).
