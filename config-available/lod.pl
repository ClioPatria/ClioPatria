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

:- module(conf_lod, []).
:- use_module(api(lod)).
:- use_module(library(http/http_dispatch)).

/** <module> Configure Linked Data (LOD) access

Load the linked-data server and the   library to register HTTP handlers.
and then register your LOD areas and/or  handlers for locations that are
redirected from e.g., http://www.purl.org. Multiple   handlers can point
to lod_api/1, but one handler should not be  a prefix of another one (as
in /rdf/ and /rdf/time/). The first   example  assumes that requests for
RDF URIs arrive at this server directly   or through a proxy. The latter
assumes that /mydata/ on purl.org is   redirected  to /purl/rdf/ on this
server and all RDF URIs start with http://www.purl.org/mydata/

The bounded_description(cbd) option selects the  default Concise Bounded
Description.  The  alternative  is  =scbd=   (Symetric  Concise  Bounded
Description), which also includes  triples  that   have  the  target  as
_object_.

@see cliopatria(api/lod)
*/

% The bounded_description option is one of `cbd` (Consise Bounded
% Description) or `scbd` (Symmetric CBD).  The latter includes triples
% that have the target resource as object.

% Use this if the URIs resolve directly to this server
%:- http_handler('/rdf',
%                lod_api([ bounded_description(cbd)
%                        ]),
%                [ prefix ]).

% Use this if the URIs are redirected to this server.
%:- http_handler('/purl/rdf',
%                lod_api([ redirected_from('http://www.purl.org/mydata'),
%                          bounded_description(cbd)
%                        ]),
%                [ prefix ]).

