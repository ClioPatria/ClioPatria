/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2011, University of Amsterdam,
		   VU University Amsterdam

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

:- module(http_cors,
	  [ cors_enable/0
	  ]).
:- use_module(library(settings)).

:- setting(http:cors, list(atom), [],
	   'Enable CORS for the listed domains.  Use [*] for all domains').

/** <module> Enable CORS: Cross-Origin Resource Sharing

This small module allows  for   enabling  Cross-Origin  Resource Sharing
(CORS) for a specific request.  Typically,   this  should be enabled for
ClioPatria  services  such  as  LOD,  SPARQL  and  application  JSON/XML
services you want to have  useable  from   browser  client  code that is
loaded from another domain.

Because CORS is a security risc (see  below), it is disabled by default,
but can be easily through the ClioPatria web-configuration.

Services for which CORS is relevant must   call cors_enable/0 as part of
the HTTP response.  E.g.:

  ==
  my_handler(Request) :-
	....,
	cors_enable,
	reply_json(Response, []).
  ==

@see	http://en.wikipedia.org/wiki/Cross-site_scripting for understanding
	Cross-site scripting.
@see	http://www.w3.org/TR/cors/ for understanding CORS
*/


cors_enable :-
	setting(http:cors, List),
	List \== [], !,
	format('Access-Control-Allow-Origin: ', []),
	write_domains(List),
	nl.
cors_enable.				% CORS not enabled

write_domains([]).
write_domains([H|T]) :-
	write(H),
	(   T == []
	->  true
	;   write(' '),
	    write_domains(T)
	).
