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

:- module(cp_page, []).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(components(menu)).
:- use_module(components(simple_search)).
:- use_module(version).

:- multifile
	user:body//2.

user:body(cliopatria(_), Body) -->
	html(body(class('yui-skin-sam'),
		  [ div(id(sidebar), \cp_menu),
		    \simple_search_form,
		    br(clear(all)),
		    div(id(content), Body),
		    br(clear(all)),
		    div(id(address), \address)
		  ])).

address -->
	{ git_version(CP_Version)
	},
	html_requires(css('cliopatria.css')),
	html([ address(class(cliopatria),
		       'ClioPatria ~w'-[CP_Version])
	     ]).
