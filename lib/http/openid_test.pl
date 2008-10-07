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

:- asserta(file_search_path(library, '..')).

:- use_module(http_openid).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/http_error')).
:- use_module(library('http/thread_httpd')).
:- use_module(library('http/html_write')).
:- use_module(library(plunit)).

:- debug(openid(_)).
%:- debug(http(_)).

consumer(Port) :-
	http_server(http_dispatch,
		    [ port(Port)
		    ]).


assoc :-
	openid_associate('http://localhost:8001/openid/server', Handle, Assoc),
	writeln(Handle-Assoc).

%%	secret(+Request) is det.
%
%	Example of a handler that requires an  OpenID login. If the user
%	is not logged it, it will be  redirected to the login page, from
%	there to the OpenID server and back here. All this is completely
%	transparent to us.

:- http_handler(root('secret'), secret, []).

secret(Request) :-
	openid_user(Request, User, []),
	reply_html_page(title('Secret'),
			[ 'You\'ve reached the secret page as user ', %'
			  a(href(User), User)
			]).
	
%%	root(+Request).
%%	allow(+Request).
%
%	Shows an indirect login.

:- http_handler(root(.),	     root,				[]).
:- http_handler(root('test/verify'), openid_verify([return_to(allow)]),	[]).
:- http_handler(root('test/allow'),  allow,				[]).

root(_Request) :-
	reply_html_page(title('Demo OpenID consumer'),
			[ h1('OpenID consumer'),
			  form([ name(login),
				 action('/test/verify'),
				 method('GET')
			       ],
			       [ div([ 'OpenID: ',
				       input([ name(openid_url),
					       size(40),
					  value('http://localhost:8000/bob') % test
					     ]),
				       input([type(submit), value('Verify!')])
				     ])
			       ]),
			  p([ 'Or go directly to the ', a(href=secret, 'secret page') ])
			]).

		   
allow(Request) :-
	openid_authenticate(Request, Server, Identity, ReturnTo),
	reply_html_page(title('Success'),
			[ h1('OpenID login succeeded'),
			  p([ 'The OpenID server ',
			      a(href(Server),Server),
			      ' verified you as ',
			      a(href(Identity), Identity)
			    ]),
			  p(['Redirect to ', a(href(ReturnTo), ReturnTo)])
			]).


		 /*******************************
		 *	   OpenID SERVER	*
		 *******************************/

:- http_handler(root('user/'),	user_page,	   [prefix]).
:- http_handler(openid(server),	openid_server([]), []).

%%	user_page(+Request) is det.
%
%	Generate a page for user below /user/<user>.

user_page(Request) :-
	http_openid:current_root_url(Request, Root),
	atom_concat(Root, 'openid/server', Me),
	memberchk(path(Path), Request),
	atom_concat('/user/', User, Path),
	reply_html_page([ link([ rel('openid.server'),
				 href(Me)
			       ]),
			  title('OpenID page of ~w'-[User])
			],
			h1('OpenID page of ~w'-[User])).
		   

		 /*******************************
		 *		DEBUG		*
		 *******************************/

:- http_handler(root(.), print_request, [prefix]).

print_request(Request) :-
	format('Content-type: text/plain~n~n'),
	pp(Request).
