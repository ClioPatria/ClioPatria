/*  $Id$

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2008, University of Amsterdam

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
	    serql_server_property/1,	% -Property
	    serql_server_set_property/1	% +Property
	  ]).
:- use_module(library(settings)).
:- use_module(http_data).
:- use_module(http_sparql).
:- use_module(http_user).
:- use_module(http_admin).
:- use_module(user_db).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(thread_pool)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(time)).
:- use_module(library(error)).
:- use_module(library(settings)).

:- if(exists_source(library(http/http_log))).
:- use_module(library(http/http_log)).
:- endif.

:- setting(sparql:max_clients, nonneg, 100,
	   'Maximum number of concurrent requests').
:- setting(sparql:stack_size, nonneg, 1000,
	   'Size of the global stack in mega-bytes').


:- dynamic
	start_time/1,			% Stamp
	loading/0,
	loading_done/2.			% Graphs, Total

%%	serql_server(?Port, +Options)
%
%	Start Semantic Web Query server at Port.  Options are passed to
%	http_server/2.

serql_server(Port, Options) :-
	create_pools,
	http_server(serql_reply,
                    [ port(Port),
                      timeout(60),
		      keep_alive_timeout(1)
                    | Options
                    ]),
	get_time(Time),
	assert(start_time(Time)).

serql_reply(_Request) :-
	loading, !,
	rdf_statistics(triples(Triples)),
	(   loading_done(Nth, Total)
	->  Extra = [ '; ~D of ~D graphs.'-[Nth, Total] ]
	;   Extra = [ '.' ]
	),
	HTML = p([ 'This service is currently restoring its ',
		   'persistent database.', br([]),
		   'Loaded ~D triples'-[Triples]
		 | Extra
		 ]),
	throw(http_reply(unavailable(HTML))).
serql_reply(Request) :-
	http_dispatch(Request).

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

%%	serql_server_set_property(+Term) is det.
%
%	Set server properties.  Currently only supports loading(Bool).

serql_server_set_property(loading(Bool)) :- !,
	must_be(boolean, Bool),
	(   Bool == true
	->  assert(loading)
	;   retractall(loading)
	).
serql_server_set_property(P) :-
	domain_error(serql_server_property, P).

%%	create_pools
%
%	Create required thread-pools

create_pools :-
	setting(sparql:max_clients, Count),
	setting(sparql:stack_size, MB),
	Global is MB * 1024,
	Trail is MB * 1024,
	thread_pool_create(sparql_query, Count,
			   [ global(Global),
			     trail(Trail)
			   ]).


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile
	user:message_hook/3.

user:message_hook(rdf(restore(_, done(_DB, _T, _Count, Nth, Total))),
		  _Kind, _Lines) :-
	retractall(loading_done(_,_)),
	assert(loading_done(Nth, Total)).

