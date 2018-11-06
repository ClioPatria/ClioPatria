/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2014-2015, University of Amsterdam
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

:- module(conf_pengines, []).
:- if(exists_source(library(pengines))).
:- use_module(api(pengines)).

/** <module> Configure Pengines access

Provide access to ClioPatria's RDF store using pengines.

@see http://www.swi-prolog.org/pldoc/package/pengines.html
@see http://cliopatria.swi-prolog.org/packs/swish
*/

% Maximum time a pengine may execute
% :- set_setting_default(pengines:time_limit, 300).

% Only allow connections from localhost.  Use `[*]` to allow access from
% anywhere.  Note that the cpack `swish` also depends on this setting.
:- set_setting_default(pengines:allow_from, ['127.0.0.1']).

% Deny hosts/networks.  Deny rules are applied after allow rules, i.e.,
% you are granted access if you are allowed and not denied.
% :- set_setting_default(pengines:deny_from,  []).

% DO NOT REMOVE THIS :- endif.
:- endif.
