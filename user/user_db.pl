/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007-2010, University of Amsterdam,
			      VU University Amsterdam.

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
:- use_module(library(http/http_session)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_openid)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_authenticate)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(library(error)).
:- use_module(library(uri)).
:- use_module(library(debug)).
:- use_module(library(persistency)).
:- use_module(openid).

/** <module> User administration

Core user administration. The  user  administration   is  based  on  the
following:

	* A persistent fact user/2
	* A dynamic fact logged_in/3
	* Session management

@see	preferences.pl implements user preferences
@see	openid.pl implements OpenID server and client
*/

:- dynamic
	logged_in/3,			% Session, User, Time
	user/2,				% Name, Options
	denied/1.			% Deny to all users


		 /*******************************
		 *	  USER DATABASE		*
		 *******************************/

:- persistent
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
	assert_user(Name, Options).

%%	user_del(+Name)
%
%	Delete named user from user-database.

user_del(Name) :-
	must_be(atom, Name),
	(   user(Name, _)
	->  retractall_user(Name, _)
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
		retractall_user(Name, _),
		assert_user(Name, [Prop|NewProps])
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
	assert_grant_openid_server(Server, Options).


%%	openid_del_server(+Server)
%
%	Delete registration of an OpenID server.

openid_del_server(Server) :-
	retractall_grant_openid_server(Server, _).


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
		retractall_grant_openid_server(Server, _),
		assert_grant_openid_server(Server, [Prop|NewProps])
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
%	refer to a server using its domain.   The actual server may be a
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
	uri_host(Server, SHost),
	uri_host(Registered, RHost),
	atomic_list_concat(SL, '.', SHost),
	atomic_list_concat(RL, '.', RHost),
	append(_, RL, SL), !.

uri_host(URI, Host) :-
	uri_components(URI, CL),
	uri_data(authority, CL, Authority),
	uri_authority_components(Authority, AC),
	uri_authority_data(host, AC, Host).

%%	openid_server_property(+Server, +Property) is semidet.
%%	openid_server_property(+Server, -Property) is nondet.
%
%	True if OpenID Server has Property.
%
%	@see openid_server_properties/2.

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
	    openid_server_properties(Server, ServerProperties)
	->  Properties = [ type(openid),
			   openid(OpenID),
			   openid_server(Server)
			 | ServerProperties
			 ]
	),
	(   nonvar(Prop)
	->  memberchk(Prop, Properties)
	;   member(Prop, Properties)
	).
uprop(Prop, User) :-
	user(User, Properties),
	member(Prop, Properties).


user_url(User, URL) :-
	uri_is_global(User), !,
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
%	hash. Uses crypt/2. The default hashing is Unix-compatible MD5.

password_hash(Password, Hash) :-
	var(Hash), !,
	phrase("$!$", HashString, _),
	crypt(Password, HashString),
	atom_codes(Hash, HashString).
password_hash(Password, Hash) :-
	crypt(Password, Hash).


		 /*******************************
		 *	 LOGIN/PERMISSIONS	*
		 *******************************/

%%	logged_on(-User) is semidet.
%
%	True if User is the name of the currently logged in user.

logged_on(User) :-
	http_in_session(SessionID),
	user_property(User, session(SessionID)), !.
logged_on(User) :-
	http_current_request(Request),
	memberchk(authorization(Text), Request),
	http_authorization_data(Text, basic(User, Password)),
	validate_password(User, Password), !.


%%	logged_on(-User, +Default) is det.
%
%	Get the current user or  unify   User  with  Default. Typically,
%	Default is =anonymous=.

logged_on(User, Default) :-
	(   logged_on(User0)
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
	open_session(Session),
	retractall(logged_in(_, Session, _)),
	assert(logged_in(Session, User, Time)),
	debug(login, 'Login user ~w on session ~w', [User, Session]).


%%	logout(+User) is det.
%
%	Logout the specified user

logout(User) :-
	must_be(atom, User),
	close_session,
	retractall(logged_in(_Session, User, _Time)),
	debug(login, 'Logout user ~w', [User]).

% Use new session management if available.

:- if(current_predicate(http_open_session/2)).

:- http_set_session_options([ create(noauto)
			    ]).
open_session(Session) :-
	http_open_session(Session, [renew(true)]).
close_session :-
	(   http_in_session(SessionId)
	->  http_close_session(SessionId)
	;   true
	).

:- else.

open_session(Session) :-
	http_session_id(Session).
close_session.

:- endif.

