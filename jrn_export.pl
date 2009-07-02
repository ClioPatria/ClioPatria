/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(rdf_jrn_export, []).
:- use_module(library('semweb/rdf_persistency')).
:- use_module(user_db).
:- use_module(http_admin).
:- use_module(http_user).
:- use_module(library('http/http_parameters')).
:- use_module(library('http/mimetype')).
:- use_module(library('http/http_dispatch')).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module exports the journal files   defined in rdf_persistency. This
is will be used to synchronise servers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- http_handler(serql(list_journals), list_journals, []).
:- http_handler(serql(journal),	      journal,	     []).

%%	list_journals(+Request)
%
%	HTTP handler to list the available journals from the persistent
%	store as an XML document.  Here is an example document:
%
%	==
%	<journals>
%	  <journal db="http://www.example.org/db1"/>
%	  <journal db="http://www.example.org/db2"/>
%	  ...
%	</journals>
%	==
%
%	@see /journal

list_journals(_Request) :- !,
	authorized(read(default, list_journals)),
	format('Content-type: text/xml~n~n'),
	format('<journals>~n'),
	forall(rdf_journal_file(DB, _File),
	       format('  <journal db="~w"/>~n', [DB])),
	format('</journals>~n').

%%	journal(+Request)
%
%	HTTP handler that serves the journal for an RDF named graph as a
%	Prolog text. If no journal  is   available  for  the given named
%	graph, it returns a 404 page.
%
%	@see /list_journals


journal(Request) :- !,
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
