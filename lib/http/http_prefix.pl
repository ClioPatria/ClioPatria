/*  This file is part of ClioPatria.

    Author:
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

:- module(http_prefix,
	  [ http_setup_prefix_redirect/0
	  ]).
:- use_module(library('http/http_dispatch')).
:- use_module(library(settings)).
:- use_module(library(lists)).

/** <module> Set common prefix for all URLs

This module manages the setting  http:prefix   to  redirect all requests
below prefix to the root. It can be   used  to relocate an entire server
under a new root.

@author	Jan Wielemaker
*/

:- setting(http:prefix, atom, env('SERQL_PREFIX', ''), 
	   'Root of all URLs').


%%	http_setup_prefix_redirect is det.
%
%	If setting http:prefix is  active,  setup   an  HTTP  handler to
%	remove the prefix.

http_setup_prefix_redirect :-
	remove_prefix_handler,
	setting(http:prefix, Prefix),
	Prefix \== '', !,
	(   sub_atom(Prefix, _, _, 0, /)
	->  ThePrefix = Prefix
	;   atom_concat(Prefix, /, ThePrefix)
	),
	http_handler(prefix(ThePrefix), redirect_prefix(ThePrefix), []).
http_setup_prefix_redirect.

remove_prefix_handler :-
	(   http_current_handler(Path, redirect_prefix(_)),
	    http_delete_handler(Path),
	    fail ; true
	).

%%	redirect_prefix(+Prefix, +Request).
%
%	Handle paths below Prefix by removing Prefix and call http_dispatch/1
%	on the result.

redirect_prefix(Prefix, Request) :-
	select(path(Path0), Request, Request1),
	atom_concat(Prefix, Path1, Path0),
	atom_concat('/', Path1, Path),
	debug(http_prefix, 'Redirected ~q --> ~q', [Path0, Path]),
	http_dispatch([path(Path),x_redirected_path(Path0)|Request1]).

:- initialization
   http_setup_prefix_redirect.

% allow for dynamic changes of the setting

:- listen(settings(changed(http:prefix, _, _)),
	  http_setup_prefix_redirect).
