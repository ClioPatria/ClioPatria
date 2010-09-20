/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam,
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

:- module(user_preferences,
	  [ user_preference/2		% ?Preference, ?Value
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(cliopatria(hooks)).

:- rdf_register_ns(user, 'http://www.swi-prolog.org/cliopatria/user/').

/** <module> User preference handling

This module defines _access_  to  user-preferences.   The  API  for  the
preference system is based on RDF. This   allows for using the RDF-store
for storing preferences, typically using the  OpenID URI as subject. The
actual storage infrastructure must be  implemented   using  hooks in the
=cliopatria=      module:      cliopatria:user_preference_db/2       and
cliopatria:user_preference_default/2.

Preferences are defines in the RDF   namespace with prefix =user=, which
expands to:

	==
	http://www.swi-prolog.org/cliopatria/user/
	==
*/

:- rdf_meta
	user_preference(r,o),
	builtin_default(r,o).

%%	user_preference(?Preference:resource, ?Value:object) is nondet.
%
%	True if Preference has Value.  Preference and Value use the RDF
%	data-representation.  E.g.,
%
%	    ==
%	    ?- user_preference(user:lang, literal(Lang))
%
%	    Lang = en
%	    ==
%
%	@see cliopatria:user_preference_db/2 provides the storage hook
%	@see cliopatria:user_preference_default/2 provides a hook for
%	defaults
%	@see builtin_default/2 provides built-in defaults

user_preference(Pref, Value) :-
	nonvar(Pref), !,
	(   cliopatria:user_preference_db(Pref, Value)
	*-> true
	;   cliopatria:user_preference_default(Pref, Value)
	*-> true
	;   builtin_default(Pref, Value)
	).
user_preference(Pref, Value) :-
	current_preference(Pref),
	user_preference(Pref, Value).

current_preference(Pref) :-
	setof(P, current_pref(P), Prefs),
	member(Pref, Prefs).

current_pref(P) :- cliopatria:user_preference_db(P, _).
current_pref(P) :- cliopatria:user_preference_default(P, _).
current_pref(P) :- builtin_default(P, _).

%%	builtin_default(?Preference, ?Value) is nondet.
%
%	Provide defaults for commonly used preferences.

builtin_default(user:lang, literal(en)).
