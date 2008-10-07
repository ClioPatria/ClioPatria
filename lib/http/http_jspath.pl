/*  This file is part of ClioPatria.

    Author:	Jan Wielemaker <J.Wielemak@uva.nl>
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

:- module(http_jspaths,
	  [ write_js_prefixes/1,	% +Options
	    write_js_locations/1	% +Options
	  ]).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_hook)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).


/** <module> Provide server paths for Javascript applications

This module creates a JavaScript declaration that provides paths defined
by the server to JavaScript  applications.   This  file provides an HTTP
service to download the paths as well   as Prolog predicates to generate
these mappings so they can be included in other documents.

@tbd	Relies on internals of library(http/http_dispatch).
@tbd	Relies on Prolog quoting of variables.  This should be fine for
	most sensible path and identifier names, but is strictly speaking
	incorrect.
*/

:- http_handler(root('js/serverpaths.js'), serverpaths, []).

:- dynamic
	served_generation/2.

%%	serverpaths(+Request)
%
%	HTTP  handler  for  /js/serverpaths.js,    serving  a  JavScript
%	declaration of all server prefixes  and handler paths-by-id. The
%	handlers  is  aware  of  modification   time  to  reduce  server
%	overhead.

serverpaths(Request) :-
	memberchk(if_modified_since(Since), Request),
	served_generation(Since, Gen),
	http_dispatch:generation(Gen), !,
	throw(http_reply(not_modified)).
serverpaths(_Request) :-
	http_dispatch:generation(Gen),
	get_time(Now),
	http_timestamp(Now, RFC),
	assert(served_generation(RFC, Gen)),
	format('Last-modified: ~w~n', [RFC]),
	format('Content-type: text/javascript~n~n'),
	format('/* Server paths as generated on ~w */~n~n', [RFC]),
	write_js_prefixes([]),
	write_js_locations([]).
	

%%	write_js_prefixes(+Options) is det.
%
%	Write a JavaScript declaration for all server paths that are
%	flagged using js(true).  Options processed:
%	
%	    * variable(+Name)
%	    Name of the JavaScript variable.  Default is =serverPrefixes=.
%	    * all(Bool)
%	    If =true= (default =false=), emit all paths instead of those
%	    flagged with js(true).
%	    
%	If these values are needed in a string, use
%	
%	==
%	with_output_to(string(S), write_js_prefixes(Options))
%	==

write_js_prefixes(Options) :-
	option(variable(Var), Options, serverPrefixes),
	option(all(All), Options, false),
	findall(Alias-Path, js_prefix(All, Alias, Path), Pairs),
	format('var ~w = {~n', [Var]),
	write_pairs(Pairs),
	format('};~n~n').

write_pairs([]).
write_pairs([Alias-Path|More]) :-
	format('  ~q:~q', [Alias, Path]),
	(   More == []
	->  nl
	;   format(',~n'),
	    write_pairs(More)
	).

js_prefix(All, Alias, Path) :-
	setof(Alias, js_alias(All, Alias), Aliases),
	member(Alias, Aliases),
	Term =.. [Alias, .],
	http_absolute_location(Term, Path, []).

js_alias(true, Alias) :- !,
	http:location(Alias, _, _).
js_alias(_, Alias) :-
	http:location(Alias, _, Options),
	option(js(true), Options).


%%	write_js_locations(+Options) is det.
%
%	Write a JavaScript declaration for all HTTP location whose option
%	list provides js(true).  Options processed:
%	
%	    * variable(+Name)
%	    Name of the JavaScript variable.  Default is =serverLocations=.
%	    * all(Bool)
%	    If =true= (default =false=), emit all paths instead of those
%	    flagged with js(true).

write_js_locations(Options) :-
	option(variable(Var), Options, serverLocations),
	option(all(All), Options, false),
	findall(ID-Path, js_location(All, ID, Path), Pairs),
	format('var ~w = {~n', [Var]),
	write_pairs(Pairs),
	format('};~n~nn').

js_location(All, ID, Path) :-
	setof(ID, js_id(All, ID), IDs),
	member(ID, IDs),
	http_location_by_id(ID, Path).

js_id(true, ID) :-
	http_dispatch:handler(_, _M:C, _, Options),
	(   option(id(ID), Options)
	->  true
	;   functor(C, ID, _Arity)
	).
js_id(_, ID) :-
	http_dispatch:handler(_, _M:C, _, Options),
	option(js(true), Options),
	(   option(id(ID), Options)
	->  true
	;   functor(C, ID, _Arity)
	).
