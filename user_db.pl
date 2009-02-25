/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@scienc.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

:- module(user_db,
	  [ set_user_database/1,	% +File

	    user_add/2,			% +Name, +Properties
	    user_del/1,			% +Name,
	    set_user_property/2,	% +Name, +Property

	    openid_add_server/2,	% +Server, +Options
	    openid_del_server/1,	% +Server
	    openid_set_property/2,	% +Server, +Property
	    openid_current_server/1,	% ?Server
	    openid_server_property/2,	% ?Server, ?Property
	    openid_server_properties/2,	% ?Server, ?Property

	    user_property/2,		% ?Name, ?Property
	    check_permission/2,		% +User, +Operation
	    validate_password/2,	% +User, +Password
	    password_hash/2,		% +Password, ?Hash

	    login/1,			% +User
	    logout/1,			% +User
	    current_user/1,		% ?User
	    logged_on/1,		% -User
	    logged_on/2,		% -User, +Default
	    ensure_logged_on/1,		% -User
	    authorized/1,		% +Action

	    deny_all_users/1		% +What
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_openid)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(library(error)).
:- use_module(library(url)).
:- use_module(library(debug)).
:- use_module(openid).
:- use_module(db).

/** <module> User administration

Core user administration. The  user  administration   is  based  on  the
following:

	* A persistent fact user/2
	* A dynamic fact logged_in/3
	* Session management

@tbd	Consider using the RDF database for login.  Maybe requires
	multiple RDF databases?

@author	Jan Wielemaker
*/

:- dynamic
	logged_in/3,			% Session, User, Time
	user/2,				% Name, Options
	denied/1.			% Deny to all users


		 /*******************************
		 *	  USER DATABASE		*
		 *******************************/

:- db_term
	user(_Name, _UserOptions),
	grant_openid_server(_Server, _ServerOptions).

%%	set_user_database(+File) is det.
%
%	Load user/2 from File.  Changes are fully synchronous.

set_user_database(File) :-
	db_attach(File, [sync(close)]).

%%	user_add(+Name, +Properties) is det.
%
%	Add a new user with given properties.

user_add(Name, Options) :-
	must_be(atom, Name),
	db_assert(user(Name, Options)).

%%	user_del(+Name)
%
%	Delete named user from user-database.

user_del(Name) :-
	must_be(atom, Name),
	(   user(Name, _)
	->  db_retractall(user(Name, _))
	;   existence_error(user, Name)
	).

%%	set_user_property(+Name, +Property) is det.
%
%	Replace Property for user Name.

set_user_property(Name, Prop) :-
	must_be(atom, Name),
	(   user(Name, OldProps)
	->  (   memberchk(Prop, OldProps)
	    ->  true
	    ;   functor(Prop, PropName, Arity),
		functor(Unbound, PropName, Arity),
		delete(OldProps, Unbound, NewProps),
		db_retractall(user(Name, _)),
		db_assert(user(Name, [Prop|NewProps]))
	    )
	;   existence_error(user, Name)
	).


%%	openid_add_server(+Server, +Options)
%
%	Register an OpenID server.

openid_add_server(Server, _Options) :-
	openid_current_server(Server), !,
	throw(error(permission_error(create, openid_server, Server),
		    context(_, 'Already present'))).
openid_add_server(Server, Options) :-
	db_assert(grant_openid_server(Server, Options)).


%%	openid_del_server(+Server)
%
%	Delete registration of an OpenID server.

openid_del_server(Server) :-
	db_retractall(grant_openid_server(Server, _)).


%%	openid_set_property(+Server, +Property) is det.
%
%	Replace Property for OpenID Server

openid_set_property(Server, Prop) :-
	must_be(atom, Server),
	(   grant_openid_server(Server, OldProps)
	->  (   memberchk(Prop, OldProps)
	    ->  true
	    ;   functor(Prop, PropName, Arity),
		functor(Unbound, PropName, Arity),
		delete(OldProps, Unbound, NewProps),
		db_retractall(grant_openid_server(Server, _)),
		db_assert(grant_openid_server(Server, [Prop|NewProps]))
	    )
	;   existence_error(openid_server, Server)
	).


%%	openid_current_server(?Server) is nondet.
%

openid_current_server(Server) :-
	grant_openid_server(Server, _).

%%	openid_server_properties(+Server, -Properties) is semidet.
%
%	Try find properties for the given server. Note that we generally
%	refer to a server using its domain.  The actjual server may be a
%	path on the server or a machine in the domain.

:- dynamic
	registered_server/2.

openid_server_properties(Server, Properties) :-
	(   registered_server(Server, Registered)
	->  grant_openid_server(Registered, Properties)
	;   grant_openid_server(Server, Properties)
	->  true
	;   grant_openid_server(Registered, Properties),
	    match_server(Server, Registered)
	->  assert(registered_server(Server, Registered))
	;   grant_openid_server(*, Properties)
	).

%%	match_server(+ServerURL, +RegisteredURL) is semidet.
%
%	True if ServerURL is in the domain of RegisteredURL.

match_server(Server, Registered) :-
	parse_url(Server, SParts),
	memberchk(host(SHost), SParts),
	parse_url(Registered, RParts),	
	memberchk(host(RHost), RParts),
	concat_atom(SL, '.', SHost),
	concat_atom(RL, '.', RHost),
	append(_, RL, SL), !.


openid_server_property(Server, Property) :-
	openid_server_properties(Server, Properties),
	(   var(Property)
	->  member(Property, Properties)
	;   memberchk(Property, Properties)
	).


		 /*******************************
		 *	     USER QUERY         *
		 *******************************/

%%	current_user(?User)
%	
%	True if User is a registered user.

current_user(User) :-
	user(User, _).

%%	user_property(?User, ?Property) is nondet.
%%	user_property(+User, +Property) is semidet.
%	
%	True if Property is a defined property on User.  In addition to
%	properties explicitely stored with users, we define:
%	
%		* session(SessionID)
%		* connection(LoginTime, Idle)
%		* url(URL)
%		Generates reference to our own OpenID server for local
%		login
%		* openid(OpenID)
%		Refers to the official OpenID (possibly delegated)
%		* openid_server(Server)
%		Refers to the OpenID server that validated the login

user_property(User, Property) :-
	nonvar(User), nonvar(Property), !,
	uprop(Property, User), !.
user_property(User, Property) :-
	uprop(Property, User).

uprop(session(SessionID), User) :-
	(   nonvar(SessionID)		% speedup
	->  !
	;   true
	),
	logged_in(SessionID, User, _).
uprop(connection(LoginTime, Idle), User) :-
	logged_in(SessionID, User, LoginTime),
	http_current_session(SessionID, idle(Idle)).
uprop(url(URL), User) :-
	user_url(User, URL).
uprop(Prop, User) :-
	nonvar(User), !,
	(   user(User, Properties)
	->  true
	;   openid_server(User, OpenID, Server),
	    openid_server_properties(Server, Properties0)
	->  Properties = [type(openid),openid(OpenID),openid_server(Server)|Properties0]
	),
	(   nonvar(Prop)
	->  memberchk(Prop, Properties)
	;   member(Prop, Properties)
	).
uprop(Prop, User) :-
	user(User, Properties),
	member(Prop, Properties).


user_url(User, URL) :-
	is_absolute_url(User), !,
	URL = User.
user_url(User, URL) :-
	openid_for_local_user(User, URL).


		 /*******************************
		 *	    MISC ROUTINES	*
		 *******************************/

%%	validate_password(+User, +Password)
%	
%	Validate the password for the given user and password.

validate_password(User, Password) :-
	user(User, Options),
	memberchk(password(Hash), Options),
	password_hash(Password, Hash).


%%	password_hash(+Password, ?Hash)
%	
%	Generate a hash from a password  or   test  a password against a
%	hash. Like Unix we add a random   2 character prefix to make the
%	same password return different  hashes   and  thus obscure equal
%	passwords.
%	
%	@tbd	Use crypt/2 from library(crypt)

password_hash(Password, Hash) :-
	var(Hash), !,
	C1 is random(0'z-0'a) + 0'a,
	C2 is random(0'z-0'a) + 0'a,
	atom_codes(Password, Codes),
	rdf_atom_md5([C1,C2|Codes], 100000, Hash0),
	atom_codes(Prefix, [C1, C2]),
	atom_concat(Prefix, Hash0, Hash).
password_hash(Password, Hash) :-
	sub_atom(Hash, 0, 2, _, Prefix),
	sub_atom(Hash, 2, _, 0, Hash0),
	atom_codes(Prefix, [C1, C2]),
	atom_codes(Password, Codes),
	rdf_atom_md5([C1,C2|Codes], 100000, Hash0).


		 /*******************************
		 *	 LOGIN/PERMISSIONS	*
		 *******************************/

%%	logged_on(-User) is det.
%
%	True if User is the name of the currently logged in user.
%
%	@error	context_error(not_logged_in)

logged_on(User) :-
	http_session_id(SessionID),
	user_property(User, session(SessionID)), !.
logged_on(_) :-
	throw(error(context_error(not_logged_in), _)).

%%	logged_on(-User, +Default) is det.
%
%	Get the current user or return Default.

logged_on(User, Default) :-
	(   http_in_session(SessionID),
	    user_property(User0, session(SessionID))
	->  User = User0
	;   User = Default
	).


%%	ensure_logged_on(-User)
%
%	Make sure we are logged in and return the current user.
%	See openid_user/3 for details.

ensure_logged_on(User) :-
	http_current_request(Request),
	openid_user(Request, User, []).


%%	authorized(+Action) is det.
%
%	validate the current user is allowed to perform Action.  Throws
%	a permission error if this is not the case.  Never fails.
%
%	@error	permission_error(http_location, access, Path)

authorized(Action) :-
	catch(check_permission(anonymous, Action), _, fail), !.
authorized(Action) :-
	ensure_logged_on(User),
	check_permission(User, Action).


%%	check_permission(+User, +Operation)
%	
%	Validate that user is allowed to perform Operation.
%	
%	@error	permission_error(http_location, access, Path)

check_permission(User, Operation) :-
	\+ denied(User, Operation),
	user_property(User, allow(Operations)),
	memberchk(Operation, Operations), !.
check_permission(_, _) :-
	http_current_request(Request),
	memberchk(path(Path), Request),
	permission_error(http_location, access, Path).

%%	denied(+User, +Operation)
%	
%	Deny actions to all users but admin.  This is a bit of a quick
%	hack to avoid loosing data in a multi-user experiment.  Do not
%	yet rely on this,

denied(admin, _) :- !, fail.
denied(_, Operation) :-
	denied(Operation).


%%	deny_all_users(+Term)
%	
%	Deny some action to all users.  See above.

deny_all_users(Term) :-
	(   denied(X),
	    X =@= Term
	->  true
	;   assert(denied(Term))
	).


%%	login(+User:atom) is det.
%	
%	Accept user as a user that has logged on into the current
%	session.

login(User) :-
	must_be(atom, User),
	get_time(Time),
	http_session_id(Session),
	retractall(logged_in(_, Session, _)),
	assert(logged_in(Session, User, Time)),
	debug(login, 'Login user ~w on session ~w', [User, Session]).


%%	logout(+User) is det.
%	
%	Logout the specified user

logout(User) :-
	must_be(atom, User),
	retractall(logged_in(_Session, User, _Time)),
	debug(login, 'Logout user ~w', [User]).
