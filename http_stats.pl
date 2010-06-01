/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam,
    			 VU University Amsterdam.

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

:- module(http_stats,
	  [ graph_triple_table//1	% +Options
	  ]).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library('http/html_write')).
:- use_module(library('http/html_head')).

:- meta_predicate
	graph_triple_table(:, ?, ?).

%%	graph_triple_table(+Options)//
%
%	HTML component that displays a table   with the triple-count per
%	graph.  Options:
%
%	    * file_action(:Action)
%	    Calls Action(File, List, Tail) at the end of each row, which
%	    allows for additional rows.  Action is called with File set
%	    to =|[title]|= for the title-row.

graph_triple_table(Options) -->
	{ meta_options(is_meta, Options, QOptions),
	  triple_stats(Total, Pairs)
	},
	html_requires(css('rdfql.css')),
	html(table([ id('triples-by-graph')
		   ],
		   [ tr([ th('Graph'),
			  th('Triples'),
			  \extra([title], QOptions)
			])
		   | \triples_by_file(Pairs, Total, odd, QOptions)
		   ])).

is_meta(file_action).

triples_by_file([], Total, _, Options) -->
	html(tr([ th([align(right), id(total)], 'Total:'),
		  \nc('~D', Total, [class(total)]),
		  \extra([total], Options)
		])).
triples_by_file([Triples-File|T], Total, OE, Options) -->
	{ oe(OE, OE2) },
	html(tr(class(OE),
		[ td(align(right), a(href(File), File)),
		  \nc('~D', Triples),
		  \extra(File, Options)
		])),
	triples_by_file(T, Total, OE2, Options).

oe(odd, even).
oe(even, odd).

triple_stats(Total, Pairs) :-
	rdf_statistics(triples(Total)),
	findall(Triples-File,
		rdf_statistics(triples_by_file(File, Triples)),
		UnsortedPairs),
	sort(UnsortedPairs, Pairs).

extra(File, Options) -->
	{ option(file_action(Action), Options) }, !,
	call(Action, File).
extra(_, _) --> [].


		 /*******************************
		 *	       UTIL		*
		 *******************************/

%%	nc(+Format, +Value)// is det.
%
%	Numeric  cell.  The  value  is    formatted   using  Format  and
%	right-aligned in a table cell (td).

nc(Fmt, Value) -->
	nc(Fmt, Value, []).

nc(Fmt, Value, Options) -->
	{ format(string(Txt), Fmt, [Value]),
	  (   memberchk(align(_), Options)
	  ->  Opts = Options
	  ;   Opts = [align(right)|Options]
	  )
	},
	html(td(Opts, Txt)).
