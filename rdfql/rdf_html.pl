/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2010, University of Amsterdam
			      VU University Amsterdam

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

:- module(rdf_html,
	  [
	  ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(http_user).
:- use_module(components(label)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library provides primitives based on the html_write library dealing
with emitting RDF related material in HTML human-readable format.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	  RESULT TABLES		*
		 *******************************/

%%	write_table(+Format, +Serialization, +Rows, +Options)
%
%	Write a result-table in human-readable HTML format
%
%	@param Format	Must be =html=

:- multifile
	rdf_io:write_table/4,
	rdf_io:write_graph/4.

rdf_io:write_table(html, _Serialization, Rows, Options) :- !,
	length(Rows, Count),
	reply_html_page(cliopatria(default),
			title('Query result'),
			[ \query_statistics([count(Count)|Options], rows),
			  \select_result_table(Rows, Options)
			]).

select_result_table(Rows, Options) -->
	html_requires(css('rdf_browse.css')),
	html_requires(css('rdfql.css')),
	html(table([ id('query-result-select'),
		     class(rdfql)
		   ],
		   [ \variables(Options)
		   | \rows(Rows, Options)
		   ])).


variables(Options) -->
	{ memberchk(variables(Vars), Options),
	  Vars =.. [_|Names]
	}, !,
	html(tr(\varnames(Names))).

varnames([]) -->
	[].
varnames([Name|T]) -->
	html(th(Name)),
	varnames(T).

rows([], _) -->
	[].
rows([H|T], Options) -->
	{ H =.. [_|Cells] },
	html(tr(\cells(Cells, Options))),
	rows(T, Options).

cells([], _) -->
	[].
cells([H|T], Options) -->
	html(td(\rdf_link(H, Options))),
	cells(T, Options).


		 /*******************************
		 *	    GRAPH OUTPUT	*
		 *******************************/

rdf_io:write_graph(html, _Serialization, Triples, Options) :-
	length(Triples, Count),
	reply_html_page(cliopatria(default),
			title('Query result'),
			[ \query_statistics([count(Count)|Options], triples),
			  \consult_result_table(Triples, Options)
			]).


consult_result_table(Triples, Options) -->
	html_requires(css('rdf_browse.css')),
	html_requires(css('rdfql.css')),
	html(table([ id('query-result-rdf'),
		     class(rdfql)
		   ],
		   [ tr([th('Subject'), th('Predicate'), th('Object')])
		   | \triples(Triples, Options)
		   ])).

triples([], _) -->
	[].
triples([H|T], Options) -->
	triple(H, Options),
	triples(T, Options).

triple(rdf(S,P,O), Options) -->
	html(tr([ td(\rdf_link(S, Options)),
		  td(\rdf_link(P, Options)),
		  td(\rdf_link(O, Options))])).


%%	query_statistics(+Options, +Units)// is det.
%
%	Emit a short line above the page summarizing resource usage
%	and result size.

query_statistics(Options, Units) -->
	{ memberchk(cputime(CPU), Options),
	  memberchk(count(Count), Options)
	}, !,
	html(div(class(query_stats),
		 'Query completed in ~3f seconds ~D ~w'-[CPU, Count, Units])).
query_statistics(_, _) -->
	[].
