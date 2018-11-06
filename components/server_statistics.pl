/*  Part of ClioPatria

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam,
                              VU University Amsterdam.
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

:- module(server_statistics,
          [ rdf_call_statistics_table//0,
            http_session_table//0,
            http_server_statistics//0,
            http_server_pool_table//0
          ]).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_session)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(user(user_db)).
:- use_module(components(basics)).

/** <module> Server statistics components

*/


%!  rdf_call_statistics_table//
%
%   Display table with RDF-call statistics

rdf_call_statistics_table -->
    { rdf_call_stats(Lookup),
      (   Lookup = [rdf(_,_,_)-_|_]
      ->  Cols = 3
      ;   Cols = 4
      )
    },
    html(table([ class(block)
               ],
               [ tr([ th(colspan(Cols), 'Indexed (SOPG)'),
                      th('Calls')
                    ]),
                 \lookup_statistics(Lookup, 1)
               ])).

rdf_call_stats(Lookup) :-
    findall(Index-Count,
            rdf_statistics(lookup(Index, Count)),
            Lookup).

lookup_statistics([], _) -->
    [].
lookup_statistics([H|T], Row) -->
    odd_even_row(Row, Next, \lookup_row(H)),
    lookup_statistics(T, Next).

lookup_row(rdf(S,P,O)-Count) -->
    html([ \i(S), \i(P), \i(O), \nc(human, Count)]).
lookup_row(rdf(S,P,O,G)-Count) -->
    html([\i(S), \i(P), \i(O), \i(G), \nc(human, Count)]).


i(I) -->
    html(td(class(instantiated), I)).


%!  http_session_table//
%
%   HTML component that writes a table of currently logged on users.

http_session_table -->
    { findall(S, session(S), Sessions0),
      sort(Sessions0, Sessions),
      Sessions \== [], !
    },
    html([ table([ class(block)
                 ],
                 [ tr([th('User'), th('Real Name'),
                       th('On since'), th('Idle'), th('From')])
                 | \sessions(Sessions, 1)
                 ])
         ]).
http_session_table -->
    html(p('No users logged in')).

%!  session(-Session:s(Idle, User, SessionID, Peer)) is nondet.
%
%   Enumerate all current HTTP sessions.

session(s(Idle, User, SessionID, Peer)) :-
    http_current_session(SessionID, peer(Peer)),
    http_current_session(SessionID, idle(Idle)),
    (   user_property(User, session(SessionID))
    ->  true
    ;   User = (-)
    ).

sessions([], _) --> [].
sessions([H|T], Row) -->
    odd_even_row(Row, Next, \session(H)),
    sessions(T, Next).

session(s(Idle, -, _SessionID, Peer)) -->
    html([td(-), td(-), td(-), td(\idle(Idle)), td(\ip(Peer))]).
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
    html([td(User), td(RealName), td(\date(OnSince)), td(\idle(Idle)), td(\ip(Peer))]).

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

ip(ip(A,B,C,D)) -->
    !,
    html('~d.~d.~d.~d'-[A,B,C,D]).
ip(IP) -->
    html('~w'-[IP]).


%!  http_server_statistics//
%
%   HTML component showing statistics on the HTTP server

http_server_statistics -->
    { findall(Port-ID, http_current_worker(Port, ID), Workers),
      group_pairs_by_key(Workers, Servers)
    },
    html([ table([ class(block)
                 ],
                 [ \servers_stats(Servers)
                 ])
         ]).

servers_stats([]) --> [].
servers_stats([H|T]) -->
    server_stats(H), servers_stats(T).

:- if(catch(statistics(process_cputime, _),_,fail)).
cputime(CPU) :- statistics(process_cputime, CPU).
:- else.
cputime(CPU) :- statistics(cputime, CPU).
:- endif.

server_stats(Port-Workers) -->
    { length(Workers, NWorkers),
      http_server_property(Port, start_time(StartTime)),
      format_time(string(ST), '%+', StartTime),
      cputime(CPU)
    },
    html([ \server_stat('Port:', Port, odd),
           \server_stat('Started:', ST, even),
           \server_stat('Total CPU usage:', [\n('~2f',CPU), ' seconds'], odd),
           \request_statistics,
           \server_stat('# worker threads:', NWorkers, odd),
           tr(th(colspan(6), 'Statistics by worker')),
           tr([ th(rowspan(2), 'Thread'),
                th(rowspan(2), 'CPU'),
                th(colspan(3), 'Stack usage')
              ]),
           tr([ th('Local'),
                th('Global'),
                th('Trail')
              ]),
           \http_workers(Workers, odd)
         ]).

server_stat(Name, Value, OE) -->
    html(tr(class(OE),
            [ th([class(p_name), colspan(3)], Name),
              td([class(value),  colspan(3)], Value)
            ])).


:- if(source_exports(library(http/http_stream), cgi_statistics/1)).
:- use_module(library(http/http_stream)).
request_statistics -->
    { cgi_statistics(requests(Count)),
      cgi_statistics(bytes_sent(Sent))
    },
    server_stat('Requests processed:', \n(human, Count), odd),
    server_stat('Bytes sent:', \n(human, Sent), even).
:- else.
request_statistics --> [].
:- endif.


http_workers([], _) -->
    [].
http_workers([H|T], OE) -->
    { odd_even(OE, OE2) },
    http_worker(H, OE),
    http_workers(T, OE2).

http_worker(H, OE) -->
    { thread_statistics(H, localused, LU),
      thread_statistics(H, globalused, GU),
      thread_statistics(H, trailused, TU),
      thread_statistics(H, cputime, CPU)
    },
    html([ tr(class(OE),
              [ td(H),
                \nc('~3f', CPU),
                \nc(human, LU),
                \nc(human, GU),
                \nc(human, TU)
              ])
         ]).

odd_even(even, odd).
odd_even(odd, even).


                 /*******************************
                 *            POOLS             *
                 *******************************/

%!  http_server_pool_table//
%
%   Display table with statistics on thread-pools.

http_server_pool_table -->
    { findall(Pool, current_thread_pool(Pool), Pools),
      sort(Pools, Sorted)
    },
    html(table([ id('http-server-pool'),
                 class(block)
               ],
               [ tr([th('Name'), th('Running'), th('Size'), th('Waiting'), th('Backlog')])
               | \server_pools(Sorted, 1)
               ])).

server_pools([], _) --> [].
server_pools([H|T], Row) -->
    odd_even_row(Row, Next, \server_pool(H)),
    server_pools(T, Next).

server_pool(Pool) -->
    { findall(P, thread_pool_property(Pool, P), List),
      memberchk(size(Size), List),
      memberchk(running(Running), List),
      memberchk(backlog(Waiting), List),
      memberchk(options(Options), List),
      option(backlog(MaxBackLog), Options, infinite)
    },
    html([ th(class(p_name), Pool),
           \nc(human, Running),
           \nc(human, Size),
           \nc(human, Waiting),
           \nc(human, MaxBackLog)
         ]).


