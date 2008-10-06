/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007, University of Amsterdam

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

:- module(rdfql_openid,
	  [ openid_for_local_user/2
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_openid)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_hook)).
:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(url)).
:- use_module(library(socket)).
:- use_module(library(debug)).
:- use_module(library(settings)).
:- use_module(http_admin).
:- use_module(user_db).


/** <module> OpenID server and client access

@author	Jan Wielemaker
*/

http:location(openid, root(openid), []).

		 /*******************************
		 *	CUSTOMISE OPENID	*
		 *******************************/

:- http_handler(openid(grant),  openid_grant, [prefix]).
:- http_handler(openid(file),   openid_file, [prefix]).

:- multifile
	http_openid:openid_hook/2.

http_openid:openid_hook(login(OpenID)) :-
	login(OpenID).
http_openid:openid_hook(logout(OpenID)) :-
	logout(OpenID).
http_openid:openid_hook(logged_in(OpenID)) :-
	http_session_id(Session),
	user_property(OpenID, session(Session)).
http_openid:openid_hook(trusted(OpenID, Server)) :-
	(   openid_server_properties(Server, _)
	->  true
	;   format(string(Msg), 'OpenID server ~w is not trusted', [Server]),
	    throw(error(permission_error(login, openid, OpenID),
			context(_, Msg)))
	).


:- http_handler(openid(login), login_page, [priority(10)]).

login_page(Request) :-
	http_parameters(Request,
			[ 'openid.return_to'(ReturnTo, [])
			]),
	reply_html_page([ title('Login'),
			  \openid_css,
			  link([ rel(stylesheet),
				 type('text/css'),
				 href(location_by_id(rdfql_css))
			       ])
			],
			[ \explain_login(ReturnTo),
			  \openid_login_form(ReturnTo, []),
			  \local_login(ReturnTo)
			]).
			
explain_login(ReturnTo) -->
	{ parse_url(ReturnTo, Parts),
	  memberchk(path(Path), Parts)
	},
	html(div(class('rdfql-login'),
		 [ p([ 'You are trying to access a page (~w) that requires authorization.'-[Path],
		       'You can either login using an ', a(href('http://www.openid.net'), 'OpenID'),
		       \explain_trusted_openid
		     ])
		 ])).

explain_trusted_openid -->
	{ openid_current_server(*) }, !,
	html(' or a local login.').
explain_trusted_openid -->
	html([' from one of our ', a(href(list_trusted_servers), 'trusted providers'), ' or a local login']).

local_login(ReturnTo) -->
	html(div(class('local-login'),
		 [ div(class('local-message'),
		       'Login with your local username and password'),
		   form([ action(location_by_id(user_login)),
			  method('GET')
			],
			[ \hidden('openid.return_to', ReturnTo),
			  div(input([name(user), size(20)])),
			  div([ input([name(password), size(20), type(password)]),
				input([type(submit), value('login')])
			      ])
			])
		 ])).

hidden(Name, Value) -->
	html(input([type(hidden), name(Name), value(Value)])).


:- http_handler(openid(list_trusted_servers), trusted_openid_servers, []).

%%	trusted_openid_servers(+Request)
%
%	HTTP handler to emit a list of OpenID servers we trust.

trusted_openid_servers(_Request) :-
	findall(S, openid_current_server(S), Servers),
	reply_html_page(title('Trusted OpenID servers'),
			[ h4('Trusted OpenID servers'),
			  p([ 'We accept OpenID logins from the following OpenID providers. ',
			      'Please register with one of them.'
			    ]),
			  ul(\trusted_openid_servers(Servers))
			]).

trusted_openid_servers([]) -->
	[].
trusted_openid_servers([H|T]) -->
	trusted_openid_server(H),
	trusted_openid_servers(T).

trusted_openid_server(*) --> !.
trusted_openid_server(URL) -->
	html(li(a(href(URL), URL))).


		 /*******************************
		 *	   OPENID SERVER	*
		 *******************************/

:- http_handler(root(user), openid_userpage, [prefix]).
:- http_handler(openid(server), openid_server([]), [prefix]).

http_openid:openid_hook(grant(Request, Options)) :-
	(   option(identity(Identity), Options),
	    option(password(Password), Options),
	    file_base_name(Identity, User),
	    validate_password(User, Password)
	->  option(trustroot(TrustRoot), Options),
	    debug(openid, 'Granted access for ~w to ~w', [Identity, TrustRoot])
	;   memberchk(path(Path), Request),
	    throw(error(permission_error(http_location, access, Path),
			context(_, 'Wrong password')))
	).


%%	openid_userpage(+Request)
%
%	Server user page for a registered user

openid_userpage(Request) :-
	memberchk(path(Path), Request),
	concat_atom(Parts, /, Path),
	append(_, [user, User], Parts), !,
	file_base_name(Path, User),
	(   current_user(User)
	->  findall(P, user_property(User, P), Props),
	    reply_html_page([ link([ rel('openid.server'),
				     href(location_by_id(openid_server))
				   ]),
			      title('OpenID page for user ~w'-[User])
			    ],
			    [ h1('OpenID page for user ~w'-[User]),
			      \user_properties(Props)
			    ])
	;   existence_error(http_location, Path)
	).


user_properties([]) -->
	[].
user_properties([H|T]) -->
	user_property(H),
	user_properties(T).

user_property(realname(Name)) --> !,
	html(div(['Real name: ', Name])).
user_property(connection(Login, IdleF)) --> !,
	{ format_time(string(S), '%+', Login),
	  Idle is round(IdleF),
	  Hours is Idle // 3600,
	  Min is Idle mod 3600 // 60,
	  Sec is Idle mod 60
	},
	html(div(['Logged in since ~s, idle for ~d:~d:~d'-
		  [S, Hours,Min,Sec]])).
user_property(_) -->
	[].


%%	openid_for_local_user(+User, -URL) is semidet.
%
%	URL is the OpenID for the local user User.

openid_for_local_user(User, URL) :-
	http_current_request(Request),
	openid_current_host(Request, Host, Port),
	http_location_by_id(openid_userpage, UserPages),
	(   Port == 80
	->  format(atom(URL), 'http://~w~w~w',
		   [ Host, UserPages, User ])
	;   format(atom(URL), 'http://~w:~w~w~w',
		   [ Host, Port, UserPages, User ])
	).



		 /*******************************
		 *	       TEST		*
		 *******************************/

:- http_handler(serql('user/form/login'), login_handler, [priority(10)]).

login_handler(_Request) :-
	ensure_logged_on(User),
	user_property(User, url(URL)),
	reload_attr(sidebar, OnLoad),
	reply_html_page(title('Login ok'),
			body(OnLoad,
			     [ h1('Login ok'),
			       p(['You''re logged on with OpenID ',
				  a(href(URL), URL)])
			     ])).
