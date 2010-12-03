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

:- module(cp_skin,
	  [ server_address//1,		% +Component
	    current_page_doc_link//0
	  ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_dispatch)).
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

:- html_resource(cliopatria,
		 [ virtual(true),
		   requires([ css('cliopatria.css')
			    ])
		 ]).

%%	user:body(+Style, :Body)// is det.
%
%	The multi-file implementation defines the overall layout of HTML
%	pages with the Style cliopatria(_).

:- multifile
	user:body//2.

user:body(cliopatria(Style), Body) -->
	cliopatria:page_body(cliopatria(Style), Body), !.
user:body(cliopatria(_), Body) -->
	cliopatria:page_body(Body), !.
user:body(cliopatria(_), Body) -->
	html_requires(cliopatria),
	html(body(class(['yui-skin-sam', cliopatria]),
		  [ div(class(menu), \cp_menu),
		    \simple_search_form([value(p(q))]),
		    br(clear(all)),
		    div(class(content), Body),
		    br(clear(all)),
		    div(class(footer), \address)
		  ])).


%%	address//
%
%	Emit an element =address= with   class  =cliopatria=. This first
%	class  the  hook  cliopatria:server_address//0.  If  this  hooks
%	fails, it calls server_address('ClioPatria').
%
%	@see version.pl

address -->
	cliopatria:server_address, !.
address -->
	server_address('ClioPatria').


%%	server_address(+Component)//
%
%	HTML component that emits the   default ClioPatria address link.
%	This provides a link to the ClioPatria   home page and the (GIT)
%	version information. ClioPatria  is  registered   with  the  GIT
%	module =|ClioPatria|= and the default server address is provided
%	by calling:
%
%	    ==
%	    	...,
%	    	server_address('ClioPatria'),
%	    	...
%	    ==
%
%	@see register_git_module/2 for registering a GIT module.

server_address(Component) -->
	html([ address(class(footer),
		       [ \component_address(Component),
			 \current_page_doc_link
		       ])
	     ]).

%%	component_address(+Name)//
%
%	The label ClioPatria as a link to its home-page on the web.

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


%%	component_version(+Name)//
%
%	Give verion information and link to detailed version info

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



%%	current_page_doc_link//
%
%	Create a link to  the  documentation   (and  from  there  to the
%	implementation) of this page. This link   is created only if the
%	library applications(help/http_help) is loaded.

current_page_doc_link -->
	{ current_predicate(http_help:page_documentation_link//1), !,
	  http_current_request(Request)
	},
	http_help:page_documentation_link(Request).
current_page_doc_link --> [].
