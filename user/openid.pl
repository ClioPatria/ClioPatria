/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam
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

:- module(cliopatria_openid,
          [ openid_for_local_user/2,            % +User, -URL
            openid_for_local_user/3             % +User, -URL, +Options
          ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_openid)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_hook)).
:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(uri)).
:- use_module(library(socket)).
:- use_module(library(debug)).
:- use_module(library(settings)).
:- use_module(user_db).


/** <module> OpenID server and client access

This module customizes login and OpenID handling for ClioPatria.

@author Jan Wielemaker
*/

http:location(openid, root(openid), []).

                 /*******************************
                 *      CUSTOMISE OPENID        *
                 *******************************/

:- http_handler(openid(grant),  openid_grant, [prefix]).

:- multifile
    http_openid:openid_hook/1.

http_openid:openid_hook(login(OpenID)) :-
    login(OpenID).
http_openid:openid_hook(logout(OpenID)) :-
    logout(OpenID).
http_openid:openid_hook(logged_in(OpenID)) :-
    logged_on(OpenID).
http_openid:openid_hook(trusted(OpenID, Server)) :-
    (   openid_server_properties(Server, _)
    ->  true
    ;   format(string(Msg), 'OpenID server ~w is not trusted', [Server]),
        throw(error(permission_error(login, openid, OpenID),
                    context(_, Msg)))
    ).


:- http_handler(openid(login), login_page, [priority(10)]).

%!  login_page(+Request)
%
%   HTTP Handler that shows both OpenID   login and local login-page
%   to the user. This handler overrules the default OpenID handler.

login_page(Request) :-
    http_open_session(_, []),               % we need sessions to login
    http_parameters(Request,
                    [ 'openid.return_to'(ReturnTo,
                                         [ description('Page to visit after login')
                                         ])
                    ]),
    reply_html_page(cliopatria(default),
                    title('Login'),
                    [ \explain_login(ReturnTo),
                      \cond_openid_login_form(ReturnTo),
                      \local_login(ReturnTo)
                    ]).

explain_login(ReturnTo) -->
    { uri_components(ReturnTo, Components),
      uri_data(path, Components, Path)
    },
    html(div(class('rdfql-login'),
             [ p([ 'You are trying to access a page (~w) that requires authorization. '-[Path],
                   \explain_open_id_login
                 ])
             ])).

explain_open_id_login -->
    { \+ openid_current_server(_) },
    !.
explain_open_id_login -->
    html([ 'You can login either as a local user',
           ' or with your ', a(href('http://www.openid.net'), 'OpenID'), '.']),
    (   { openid_current_server(*) }
    ->  []
    ;   { http_link_to_id(trusted_openid_servers, [], HREF) },
        html([ ' from one of our ', a(href(HREF), 'trusted providers')])
    ).

cond_openid_login_form(_) -->
    { \+ openid_current_server(_) },
    !.
cond_openid_login_form(ReturnTo) -->
    openid_login_form(ReturnTo, []).


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

%!  trusted_openid_servers(+Request)
%
%   HTTP handler to emit a list of OpenID servers we trust.

trusted_openid_servers(_Request) :-
    findall(S, openid_current_server(S), Servers),
    reply_html_page(cliopatria(default),
                    title('Trusted OpenID servers'),
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
                 *         OPENID SERVER        *
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


%!  openid_userpage(+Request)
%
%   Server user page for a registered user

openid_userpage(Request) :-
    memberchk(path(Path), Request),
    atomic_list_concat(Parts, /, Path),
    append(_, [user, User], Parts),
    !,
    file_base_name(Path, User),
    (   current_user(User)
    ->  findall(P, user_property(User, P), Props),
        reply_html_page(cliopatria(default),
                        [ link([ rel('openid.server'),
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

user_property(realname(Name)) -->
    !,
    html(div(['Real name: ', Name])).
user_property(connection(Login, IdleF)) -->
    !,
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


%!  openid_for_local_user(+User, -URL) is semidet.
%
%   URL is the OpenID for the local user User.

openid_for_local_user(User, URL) :-
    openid_for_local_user(User, URL, []).

openid_for_local_user(User, URL, Options) :-
    option(public_host(Host), Options),
    !,
    http_location_by_id(openid_userpage, UserPages),
    atom_concat(/, BelowRoot, UserPages),
    format(atom(URL), '~w~w/~w',
               [ Host, BelowRoot, User ]).
openid_for_local_user(User, URL, _Options) :-
    http_current_request(Request),
    openid_current_host(Request, Host, Port),
    http_location_by_id(openid_userpage, UserPages),
    (   Port == 80
    ->  format(atom(URL), 'http://~w~w/~w',
               [ Host, UserPages, User ])
    ;   format(atom(URL), 'http://~w:~w~w/~w',
               [ Host, Port, UserPages, User ])
    ).



                 /*******************************
                 *             TEST             *
                 *******************************/

:- http_handler(cliopatria('user/form/login'), login_handler, [priority(10)]).

login_handler(_Request) :-
    ensure_logged_on(User),
    user_property(User, url(URL)),
    reply_html_page(cliopatria(default),
                    title('Login ok'),
                    [ h1('Login ok'),
                      p(['You''re logged on with OpenID ',
                         a(href(URL), URL)])
                    ]).
