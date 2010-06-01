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
	  [ graph_triple_table//1,	% +Options
	    rdf_call_stat_table//0,
	    http_session_table//0,
	    http_server_statistics//0
	  ]).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library('http/http_session')).
:- use_module(library('http/thread_httpd')).
:- use_module(library('http/html_write')).
:- use_module(library('http/html_head')).
:- use_module(user_db).

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
	html(table([ id('triples-by-graph'),
		     class(rdfql)
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


%%	rdf_call_stat_table//
%
%	Display table with RDF-call statistics

rdf_call_stat_table -->
	{ rdf_call_stats(Lookup) },
	html_requires(css('rdfql.css')),
	html(table([ id('rdf-call-stats'),
		     class(rdfql)
		   ],
		   [ tr([ th(colspan(3), 'Indexed'),
			  th('Calls')
			]),
		     \lookup_statistics(Lookup)
		   ])).

rdf_call_stats(Lookup) :-
	findall(Index-Count,
		rdf_statistics(lookup(Index, Count)),
		Lookup).

lookup_statistics([]) -->
	[].
lookup_statistics([rdf(S,P,O)-Count|T]) -->
	html(tr([ td(S), td(P), td(O), \nc('~D', Count)])),
	lookup_statistics(T).


%%	http_session_table//
%
%	HTML component that writes a table of currently logged on users.

http_session_table -->
	{ findall(S, session(S), Sessions0),
	  sort(Sessions0, Sessions),
	  Sessions \== [], !
	},
	html([ table([ id('http-session-table'),
		       class(rdfql)
		     ],
		     [ tr([th('User'), th('Real Name'),
			   th('On since'), th('Idle'), th('From')])
		     | \sessions(Sessions)
		     ])
	     ]).
http_session_table -->
	html(p('No users logged in')).

session(s(Idle, User, SessionID, Peer)) :-
	http_current_session(SessionID, peer(Peer)),
	http_current_session(SessionID, idle(Idle)),
	(   user_property(User, session(SessionID))
	->  true
	;   User = (-)
	).

sessions([]) --> [].
sessions([H|T]) --> session(H), sessions(T).

session(s(Idle, -, _SessionID, Peer)) -->
	html(tr([td(-), td(-), td(-), td(\idle(Idle)), td(\ip(Peer))])).
session(s(Idle, User, _SessionID, Peer)) -->
	{  (   user_property(User, realname(RealName))
	   ->  true
	   ;   RealName = '?'
	   ),
	   (   user_property(User, connection(OnSince, _Idle))
	   ->  true
	   ;   OnSince = 0
	   )
	},
	html(tr([td(User), td(RealName), td(\date(OnSince)), td(\idle(Idle)), td(\ip(Peer))])).

idle(Time) -->
	{ Secs is round(Time),
	  Min is Secs // 60,
	  Sec is Secs mod 60
	},
	html('~`0t~d~2|:~`0t~d~5|'-[Min, Sec]).

date(Date) -->
	{ format_time(string(S), '%+', Date)
	},
	html(S).

ip(ip(A,B,C,D)) --> !,
	html('~d.~d.~d.~d'-[A,B,C,D]).
ip(IP) -->
	html('~w'-[IP]).


%%	http_server_statistics//
%
%	HTML component showing statistics on the HTTP server

http_server_statistics -->
	{ findall(Port-ID, http_current_worker(Port, ID), Workers),
	  group_pairs_by_key(Workers, Servers),
	  statistics(heapused, Heap)
	},
	html([ table([ id('http-server-statistics'),
		       class(rdfql)
		     ],
		     [ \servers_stats(Servers),
		       tr([ th([align(right), colspan(3)], 'Heap memory:'),
			    \nc('~D', Heap, [align(left), colspan(3)])
			  ])
		     ])
	     ]).

servers_stats([]) --> [].
servers_stats([H|T]) -->
	server_stats(H), servers_stats(T).

server_stats(Port-Workers) -->
	{ length(Workers, NWorkers),
	  http_server_property(Port, start_time(StartTime)),
	  format_time(string(ST), '%+', StartTime)
	},
	html([ tr([ th([align(right), colspan(3)], 'Port:'),
		    td(colspan(3), Port)
		  ]),
	       tr([ th([align(right), colspan(3)], 'Started:'),
		    td(colspan(3), ST)
		  ]),
	       tr([ th([align(right), colspan(3)], '# worker threads:'),
		    td(colspan(3), NWorkers)
		  ]),
	       tr(th(colspan(6), 'Statistics by worker')),
	       tr([ th('Thread'),
		    th('CPU'),
		    th(''),
		    th('Local'),
		    th('Global'),
		    th('Trail')
		  ]),
	       \http_workers(Workers)
	     ]).


http_workers([]) -->
	[].
http_workers([H|T]) -->
	{ thread_statistics(H, locallimit, LL),
	  thread_statistics(H, globallimit, GL),
	  thread_statistics(H, traillimit, TL),
	  thread_statistics(H, localused, LU),
	  thread_statistics(H, globalused, GU),
	  thread_statistics(H, trailused, TU),
	  thread_statistics(H, cputime, CPU)
	},
	html([ tr([ td(rowspan(2), H),
		    \nc('~3f', CPU, [rowspan(2)]),
		    th('In use'),
		    \nc('~D', LU),
		    \nc('~D', GU),
		    \nc('~D', TU)
		  ]),
	       tr([ th('Limit'),
		    \nc('~D', LL),
		    \nc('~D', GL),
		    \nc('~D', TL)
		  ])
	     ]),
	http_workers(T).


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
