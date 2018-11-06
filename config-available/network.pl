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

:- module(conf_network, []).
:- use_module(library(settings)).

/** <module> Configure the HTTP server

Change the default port on which the HTTP server listens. If the port is
an integer, the server is  available   from  all  interfaces. The common
alternative is `localhost:Port`, binding  the   server  to  th localhost
network only. This is particularly intertesting if  the same host runs a
reverse proxy.

If host-detection does not work  or  this   server  is  behind a reverse
proxy, you may also need the   public_host/public_port  settings to tell
ClioPatria where it can be reached.

The =prefix= setting rebases all paths on   the  server to the indicated
path. Note that the prefix has *no* trailing /. E.g. a setting =|/demo|=
changes the root of the server to  =|/demo/|=. Rebasing a server is only
possible if internal path dependencies use   the  HTTP path mechanism to
find paths for internal services.

The setting =workers= sets the number of   HTTP  worker threads. See the
link below for more info.

@see    localhost.pl
@see    http_location_by_id/2 and http_link_to_id/3 for finding the
        locations of internal services.
@see    http://www.swi-prolog.org/howto/http/HTTPScale.html for more
        info on server scalability.
*/

% :- set_setting_default(http:port, 8080).
% :- set_setting_default(http:port, localhost:8080).
% :- set_setting_default(http:public_host, 'www.example.org').
% :- set_setting_default(http:public_port, 80).
% :- set_setting_default(http:prefix, '/demo').
% :- set_setting_default(http:workers, 16).

% Allow  CORS  enabled  access  by    default.   Needed  for  JavaScript
% applications loaded from other sites  to   access  the SPARQL endpoint
% other JSON or XML APIs of ClioPatria.   One of the features that needs
% it is YASGUI for accessing the server on http://localhost

:- set_setting_default(http:cors, [*]).
