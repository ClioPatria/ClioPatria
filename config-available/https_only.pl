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

:- module(conf_https_only, []).
:- use_module(library(option)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(cliopatria(cliopatria)).

/** <module> Configure the HTTPS server

To enable HTTPS, create a directory =http= inside the =|config-enabled|=
directory of the application and add the following files:

    $ =|server-cert.pem|= :
      Contains the server certificate.  Passed as certificate_file(File)
      to the ssl_context/3 predicate. This file may be omitted, in
      which case the =|server-key.pem|= is also passed using the
      key_file(+File) option.
    $ =|server-key.pem|= :
      Contains the private key for the server.  Passed as key_file(File)
      option of ssl_context/3.
    $ =|passwd|= :
      Needs to hold the password if the private key is protected
      with a password.  Passed using the password(Password) option
      of ssl_context/3.

@see    http://www.swi-prolog.org/pldoc/doc_for?object=ssl_context/3 for
        a description of these files.
*/

% uncomment the following to add a server that redirects requests from a
% plain HTTP port to the HTTPS port. This configuration assumes there is
% no reverse proxy between the public   network and ClioPatria. If there
% is, this redirect as well as HTTPS   handling is typically done by the
% proxy server.

% :- initialization cp_after_load(plain_http_server).

plain_http_server :-
    plain_http_server([port(5020)]).

plain_http_server(Options) :-
    option(port(Port), Options),
    http_server_property(Port, goal(redirect_to_https)),
    !.
plain_http_server(Options) :-
    http_server(redirect_to_https,
                Options).

redirect_to_https(Request) :-
    option(host(Host), Request),
    option(request_uri(ReqURI), Request),
    http_server_property(Port, scheme(https)),
    format(string(URL), 'https://~w:~w~w', [Host, Port, ReqURI]),
    http_redirect(see_other, URL, Request).
