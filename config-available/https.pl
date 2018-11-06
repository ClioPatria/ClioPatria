/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam
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

:- module(conf_https, []).
:- use_module(library(settings)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

/** <module> Provide HTTPS

This plugin module makes the server available under https (aka http over
SSL).  The default port for HTTPS usage is 443.

@see etc/README.txt for creating SSL certificates
*/

:- setting(https:port, integer, 1443,
           'Port to use for https connections').

:- set_setting_default(http:public_host,   localhost).
:- set_setting_default(http:public_port,   setting(https:port)).
:- set_setting_default(http:public_scheme, https).

start_https :-
    setting(https:port, Port),
    http_server(http_dispatch,
                [ port(Port),
                  ssl([ host('localhost'),
                        cacert_file('etc/demoCA/cacert.pem'),
                        certificate_file('etc/server/server-cert.pem'),
                        key_file('etc/server/server-key.pem'),
                        password('apenoot1')
                      ])
                ]).

:- initialization
    start_https.


