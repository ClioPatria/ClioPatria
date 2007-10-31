/*  This file is part of ClioPatria.

    Author:	Jan Wielemaker
    HTTP:	http://e-culture.multimedian.nl/
    GITWEB:	http://gollem.science.uva.nl/git/ClioPatria.git
    GIT:	git://gollem.science.uva.nl/home/git/ClioPatria.git
    GIT:	http://gollem.science.uva.nl/home/git/ClioPatria.git
    Copyright:  2007, E-Culture/MultimediaN

    ClioPatria is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    ClioPatria is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ClioPatria.  If not, see <http://www.gnu.org/licenses/>.
*/

:- module(http_host,
	  [ http_current_host/4		% +Request, -Host, -Port, +Options
	  ]).
:- use_module(library('http/thread_httpd')).
:- use_module(library(socket)).
:- use_module(library(option)).
:- use_module(library(settings)).

:- setting(http:public_host, atom, '',
	   'Name the outside world can use to contact me').

%%	http_current_host(+Request, -Hostname, -Port, Options) is det.
%
%	Current global host and port of the HTTP server.  This is the
%	basis to form absolute address, which we need for redirection
%	based interaction such as the OpenID protocol.  Options are:
%	
%	    * global(+Bool)
%	    If =true= (default =false=), try to replace a local hostname
%	    by a world-wide accessible name.

http_current_host(Request, Host, Port, Options) :-
	(   memberchk(x_forwarded_host(Forwarded), Request)
	->  Port = 80,
	    primary_forwarded_host(Forwarded, Host)
	;   memberchk(host(Host0), Request),
	    (	option(global(true), Options, false)
	    ->	global_host(Host0, Host)
	    ;	Host = Host0
	    ),
	    option(port(Port), Request, 80)
	->  true
	;   gethostname(Host),
	    http_current_server(_Pred, Port)		% TBD: May be more
	).


%%	primary_forwarded_host(+Spec, -Host) is det.
%
%	x_forwarded host contains multiple hosts seperated   by  ', ' if
%	there are multiple proxy servers in   between.  The first one is
%	the one the user's browser knows about.

primary_forwarded_host(Spec, Host) :-
	sub_atom(Spec, B, _, _, ','), !,
	sub_atom(Spec, 0, B, _, Host).
primary_forwarded_host(Host, Host).


%%	global_host(+HostIn, -Host)
%
%	Globalize a hostname. Used if we need  to pass our hostname to a
%	client and expect the client to be   able to contact us. In this
%	case we cannot use a  name  such   as  `localhost'  or the plain
%	hostname of the machine. We assume   (possibly  wrongly) that if
%	the host contains a '.', it is globally accessible.
%	
%	If the heuristics used by  this   predicate  do not suffice, the
%	setting http:public_host can be used to override.

global_host(_, Host) :-
	setting(http:public_host, PublicHost), PublicHost \== '', !,
	Host = PublicHost.
global_host(localhost, Host) :- !,
	gethostname(Host).
global_host(Local, Host) :-
	sub_atom(Local, _, _, _, '.'), !,
	Host = Local.
global_host(Local, Host) :-
	tcp_host_to_address(Local, IP),
	tcp_host_to_address(Host, IP).


