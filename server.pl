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
:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library(time)).

:- if(exists_source(library(http/http_log))).
:- use_module(library(http/http_log)).
:- endif.

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




