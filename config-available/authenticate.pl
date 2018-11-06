/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2015-2018, University of Amsterdam
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

:- module(conf_authenticate, []).
:- use_module(library(http/http_wrapper), []).
:- use_module(library(http/http_authenticate)).
:- use_module(user(user_db)).

/** <module> Protect entire server

This  config  file  protects  the  entire    server   using  basic  HTTP
authentication. This may be combined with configuring HTTPS as described
in https.pl.

ClioPatria will not force login if  no   users  are yet defined. In this
case you will be redirected to the `create admin user page'.  If youn do
not want that, delete the second clause.

@tbd    If you use this login mechanism, you cannot logout.  Maybe we
        should implement http://stackoverflow.com/questions/233507
*/

:- multifile
    http:request_expansion/2.

http:request_expansion(Request, Request) :-
    memberchk(authorization(Text), Request),
    !,
    (   http_authorization_data(Text, basic(User, Password)),
        validate_password(User, Password)
    ->  true
    ;   throw(http_reply(authorise(basic, 'ClioPatria')))
    ).
http:request_expansion(Request, Request) :-
    \+ current_user(_),
    !.                  % allow if no users are defined
http:request_expansion(Request, Request) :-
    throw(http_reply(authorise(basic, 'ClioPatria'))).
