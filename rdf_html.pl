/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2004, University of Amsterdam

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
:- use_module(library('http/html_write')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).


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
	phrase(page(title('Query result'),
		    [ \query_statistics([count(Count)|Options], rows),
		      table([ align(center),
			      border(1)
			    ],
			    [ \variables(Options)
			    | \rows(Rows, Options)
			    ])
		    ]), HTML),
	format('Content-type: text/html~n~n'),
	print_html(HTML).

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
	html(td(\object(H, Options))),
	cells(T, Options).

	
		 /*******************************
		 *	    GRAPH OUTPUT	*
		 *******************************/

rdf_io:write_graph(html, _Serialization, Triples, Options) :-
	length(Triples, Count),
	phrase(page(title('Query result'),
		    [ \query_statistics([count(Count)|Options], triples),
		      table([ align(center),
			      border(1)
			    ],
			    [ tr([th('Subject'), th('Predicate'), th('Object')])
			    | \triples(Triples, Options)
			    ])
		    ]), HTML),
	format('Content-type: text/html~n~n'),
	print_html(HTML).

triples([], _) -->
	[].
triples([H|T], Options) -->
	triple(H, Options),
	triples(T, Options).

triple(rdf(S,P,O), Options) -->
	html(tr([ td(\resource(S, Options)),
		  td(\resource(P, Options)),
		  td(\object(O, Options))])).
		  
resource(R, Options) -->
	{   memberchk(resource_format(Fmt), Options)
	->  true
	;   Fmt = plain
	},
	resource(Fmt, R, Options).

resource(_, R, _) -->
	{ var(R) }, !.
resource(plain, R, _) --> !,
	html(R).
resource(_, R, _) -->
	{ sub_atom(R, 0, L, _, '__file:/'), !, % bnode
	  sub_atom(R, L, _, 0, Path),
	  file_base_name(Path, Base),
	  atom_concat('__', Base, Label)
	},
	html(Label).
resource(ns, R, _) --> !,
	(   { rdf_global_id(NS:Local, R) }
	->  html([NS, :, Local])
	;   html(R)
	).
resource(nslabel, R, _) --> !,
	{ rdfs_ns_label(R, Label)
	},
	html(Label).

object(V,_Options) -->
	{ var(V) }, !.
object(literal(type(T, V)), Options) --> !,
	html(['"', V, '"^^', \resource(T, Options)]).
object(literal(lang(L, V)), _) --> !,
	html(['"', V, '"@', L]).
object(literal(V), _) --> !,
	html(['"', V, '"']).
object(R, Options) -->
	resource(R, Options).


%%	query_statistics(+Options, +Units)// is det.
%	
%	Emit a short line above the page summarizing resource usage
%	and result size.

query_statistics(Options, Units) -->
	{ memberchk(cputime(CPU), Options),
	  sformat(T, '~2f', CPU),
	  memberchk(count(Count), Options),
	  sformat(C, '~D', Count)
	}, !,
	html(center(blockquote(i(['Query completed in ', T, ' seconds, ',
				  C, ' ', Units])))).
query_statistics(_, _) -->
	[].
