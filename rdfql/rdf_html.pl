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
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_head)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(components(label)).


/** <module> Write query-results as HTML table.

This module writes a SPARQL-table as an HTML   table.  It acts as a hook
into rdf_io.pl.
*/

		 /*******************************
		 *	  RESULT TABLES		*
		 *******************************/

%%	rdf_io:write_table(+Format, +Serialization, +Rows, +Options) is semidet.
%
%	Write a result-table in human-readable HTML format
%
%	@param Format This clause only succeeds of Format is =html=

:- multifile
	rdf_io:write_table/4,
	rdf_io:write_graph/4.

rdf_io:write_table(html, _Serialization, Rows, Options) :- !,
	length(Rows, Count),
	reply_html_page(cliopatria(default),
			title('Query result'),
			[ \query_statistics([count(Count)|Options], rows),
			  \select_result_table(Rows, Options),
			  \new_query
			]).

select_result_table(Rows, Options) -->
	html_requires(css('rdf.css')),
	html(table([ class(block)
		   ],
		   [ \variables(Options)
		   | \rows(Rows, Options, odd)
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

rows([], _, _) --> [].
rows([H|T], Options, Class) -->
	{ H =.. [_|Cells],
	  odd_even(Class, NextClass)
	},
	html(tr(class(Class), \cells(Cells, Options))),
	rows(T, Options, NextClass).

cells([], _) -->
	[].
cells([H|T], Options) -->
	cell(H, Options),
	cells(T, Options).

cell(H, _) -->
	{ var(H) }, !,
	html(td(span(class(rdf_unbound), '<unbound>'))).
cell(H, Options) -->
	html(td(\rdf_link(H, Options))).

odd_even(odd, even).
odd_even(even, odd).


		 /*******************************
		 *	    GRAPH OUTPUT	*
		 *******************************/

%%	rdf_io:write_graph(+Format, +Serialization, +Triples, +Options)
%
%	Write an RDF result-graph as an HTML table, where resources
%	are links to the ClioPatria local view.
%
%	@param Format must be =html=
%	@param Serialization is ignored
%	@param Triples is a list of rdf(S,P,O) triples
%	@param Options is passed to rdf_link//2.  It normally defines
%	the preferred resource representation.

rdf_io:write_graph(html, _Serialization, Triples, Options) :-
	length(Triples, Count),
	reply_html_page(cliopatria(default),
			title('Query result'),
			[ \query_statistics([count(Count)|Options], triples),
			  \consult_result_table(Triples, Options)
			]).


consult_result_table(Triples, Options) -->
	html_requires(css('rdf.css')),
	html(table([ class(block)
		   ],
		   [ tr([th('Subject'), th('Predicate'), th('Object')])
		   | \triples(Triples, Options, odd)
		   ])).

triples([], _, _) -->
	[].
triples([H|T], Options, Class) -->
	{ odd_even(Class, NextClass) },
	triple(H, Options, Class),
	triples(T, Options, NextClass).

triple(rdf(S,P,O), Options, Class) -->
	html(tr(class(Class),
		[ td(\rdf_link(S, Options)),
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
	html(p(class(msg_informational),
	       'Query completed in ~3f seconds ~D ~w'-[CPU, Count, Units])).
query_statistics(_, _) -->
	[].

%%	new_query//

new_query -->
	{ http_link_to_id(query_form, [], QueryForm)
	},
	html([ br(clear(all)),
	       a([class('new-query'), href(QueryForm)], 'New query')
	     ]).
