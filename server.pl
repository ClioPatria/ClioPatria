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

:- module(serql_http,
	  [ serql_server/2,		% +Port, +Options
	    serql_server_property/1	% -Property
	  ]).
:- use_module(library(settings)).
:- use_module(http_data).
:- use_module(http_sparql).
:- use_module(http_user).
:- use_module(http_admin).
:- use_module(user_db).
:- use_module(serql_log).
:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/http_prefix')).
:- use_module(library(time)).

:- dynamic
	start_time/1.

%%	serql_server(?Port, +Options)
%
%	Start Semantic Web Query server at Port.  Options are passed to
%	http_server/2.

serql_server(Port, Options) :-
	http_server(serql_reply,
                    [ port(Port),
                      timeout(60),
		      keep_alive_timeout(1)
                    | Options
                    ]),
	get_time(Time),
	assert(start_time(Time)).

%%	serql_server_property(?Property)
%	
%	Query status and attributes of the server. Defined properties
%	are:
%	
%		* port(-Port)
%		Port on which the server is running.
%		
%		* start_time(-Time)
%		TimeStamp when the server was started.

serql_server_property(port(Port)) :-
	http_current_server(serql_reply, Port).
serql_server_property(started(Time)) :-
	start_time(Time).

serql_reply(Request0) :-
	prune_request(Request0, Request),
	(   serql_log_stream(Stream)
	->  get_time(Now),
	    flag(serql_request_id, Id, Id+1),
	    statistics(cputime, CPU0),
	    log_request(Request, LogRequest),
	    catch(logged_on(User), _, User = anonymous),
	    format_time(string(HDate), '%+', Now),
	    format(Stream,
		   '/*~s*/ request(~q, ~0f, ~q, ~q).~n',
		   [HDate, Id, Now, User, LogRequest]),
	    flush_output(Stream),
	    call_cleanup(http_dispatch(Request), Reason,
			 log_completed(Reason, Id, CPU0, Stream))
	;   http_dispatch(Request)
	).

%%	prune_request(+Request, -Pruned)
%	
%	Prune some junk from the request we are not interested in anyway
%	to reduce processing time, simplify debugging and reduce the
%	logfiles.

prune_request([], []).
prune_request([H|T0], T) :-
	hidden(H), !,
	prune_request(T0, T).
prune_request([H|T0], [H|T]) :-
	prune_request(T0, T).

hidden(accept(_)).
hidden(accept_language(_)).
hidden(accept_encoding(_)).
hidden(accept_charset(_)).
hidden(referer(R)) :-			% do not want these in log
	sub_atom(R, _, _, _, password), !.

%%	log_request(+Request, -Log)
%	
%	Remove passwords from the request to avoid sending them to the
%	logfiles.

log_request([], []).
log_request([search(Search0)|T0], [search(Search)|T]) :- !,
	mask_passwords(Search0, Search),
	log_request(T0, T).
log_request([H|T0], T) :-
	nolog(H), !,
	log_request(T0, T).
log_request([H|T0], [H|T]) :-
	log_request(T0, T).

mask_passwords([], []).
mask_passwords([Name=_|T0], [Name=xxx|T]) :-
	pwd_entry(Name), !,
	mask_passwords(T0, T).
mask_passwords([H|T0], [H|T]) :-
	mask_passwords(T0, T).

pwd_entry(password).
pwd_entry(pwd0).
pwd_entry(pwd1).
pwd_entry(pwd2).

nolog(input(_)).



