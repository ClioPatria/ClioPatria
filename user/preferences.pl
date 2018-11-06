/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam,
                              VU University Amsterdam
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

:- module(user_preferences,
          [ user_preference/2           % ?Preference, ?Value
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

%!  user_preference(?Preference:resource, ?Value:object) is nondet.
%
%   True if Preference has Value.  Preference and Value use the RDF
%   data-representation.  E.g.,
%
%       ==
%       ?- user_preference(user:lang, literal(Lang))
%
%       Lang = en
%       ==
%
%   @see cliopatria:user_preference_db/2 provides the storage hook
%   @see cliopatria:user_preference_default/2 provides a hook for
%   defaults
%   @see builtin_default/2 provides built-in defaults

user_preference(Pref, Value) :-
    nonvar(Pref),
    !,
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

%!  builtin_default(?Preference, ?Value) is nondet.
%
%   Provide defaults for commonly used preferences.

builtin_default(user:lang, literal(en)).
