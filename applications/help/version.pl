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

:- module(cpa_version_help,
	  [
	  ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(version)).
:- use_module(library(occurs)).
:- use_module(library(sgml)).
:- use_module(library(lists)).

/** <module> Provide detailed version information
*/

:- http_handler(root(help/versions), version_info, []).

%%	version_info(+Request)
%
%	HTTP handler that provides detailed   information  on the loaded
%	(and registered) GIT modules.

version_info(_Request) :-
	reply_html_page(cliopatria(default),
			[ title('Version details') ],
			[ h4('GIT components'),
			  \git_components,
			  h4('Server implementation language'),
			  p(\prolog_version),
			  \about_git_versions
			]).

%%	git_components//
%
%	Component that creates a table of registered GIT modules.
%
%	@see register_git_component/2

git_components -->
	{ findall(C-V, git_component_property(C, version(V)), Pairs) },
	html(table(class('cliopatria'),
		   [ tr([ th('GIT component'), th('Version'), th('Directory') ]),
		     \git_components(Pairs)
		   ])).

git_components([]) --> [].
git_components([H|T]) -->
	git_component(H),
	git_components(T).

git_component(Name-Version) -->
	{ git_component_property(Name, directory(Dir)) },
	html(tr([td(Name), td(Version), td(Dir)])).

%%	prolog_version//
%
%	Component that emits the current version of SWI-Prolog

prolog_version -->
	{ current_prolog_flag(version_data, swi(Major, Minor, Patch, _)) },
	html([ a(href('http://www.swi-prolog.org'), 'SWI-Prolog'), ' ',
	       'version ~w.~w.~w'-[Major, Minor, Patch]
	     ]),
	(   { current_prolog_flag(version_git, GitVersion) }
	->  html([' (GIT version ', GitVersion, ')'])
	;   []
	).

%%	about_git_versions//


about_git_versions -->
	insert_html(html('git-versions.html')).

insert_html(Alias) -->
	{ absolute_file_name(Alias, Page, [access(read)]),
	  load_html_file(Page, DOM),
	  contains_term(element(body, _, Body), DOM),
	  Style = element(style, _, _),
	  findall(Style, sub_term(Style, DOM), Styles),
	  append(Styles, Body, Content)
	},
	html(Content).


