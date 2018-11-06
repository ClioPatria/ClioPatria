/*  Part of ClioPatria semantic web server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2018, University of Amsterdam
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

:- module(cp_parms,
          [
          ]).
:- use_module(library(settings)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_hook)).

/** <module> ClioPatria parameters

This file contains the locations of file-directories and web-directories
in addition to settings that can be   managed by the end-user. It should
not be necessary to modify this file:

    * Web-locations can be modified externally using http:location/3
    with an option priority(N), where N > 0. See http_absolute_location/3.

    * Settings can be changed using set_setting_default/2.

@see run.pl[.in] contains an example startup script.
*/

                 /*******************************
                 *          FILE PATHS          *
                 *******************************/

:- multifile
    user:file_search_path/2.

% ClioPatria specific ones
user:file_search_path(rdfql,            cliopatria(rdfql)).
user:file_search_path(cpack,            cliopatria(cpack)).

% Allow local file overwrites
user:file_search_path(web,              web).

% Configuration
user:file_search_path(config_https,     cp_application('config-enabled/https')).

% Package merge
user:file_search_path(cpacks,           cliopatria('.')).

user:file_search_path(library,          cpacks(lib)).
user:file_search_path(rdf,              cpacks(rdf)).
user:file_search_path(entailment,       cpacks(entailment)).
user:file_search_path(components,       cpacks(components)).
user:file_search_path(applications,     cpacks(applications)).
user:file_search_path(api,              cpacks(api)).
user:file_search_path(user,             cpacks(user)).
user:file_search_path(config_available, cpacks('config-available')).
user:file_search_path(skin,             cpacks(skin)).
user:file_search_path(web,              cpacks(web)).
user:file_search_path(css,              web(css)).
user:file_search_path(icons,            web(icons)).
user:file_search_path(yui,              web('yui/2.7.0')).
user:file_search_path(js,               web(js)).
user:file_search_path(html,             web(html)).
user:file_search_path(help,             web(help)).
user:file_search_path(tutorial,         web(tutorial)).
user:file_search_path(flint,            web('FlintSparqlEditor/sparql')).
user:file_search_path(yasqe,            web('yasqe/dist')).
user:file_search_path(yasr,             web('yasr/dist')).


                 /*******************************
                 *           HTTP PATHS         *
                 *******************************/

http:location(cliopatria,  root(.),            []).
http:location(web,         cliopatria(web),    []).
http:location(sesame,      root(servlets),     []).
http:location(sparql,      root(sparql),       []).
http:location(rdf_browser, cliopatria(browse), []).
http:location(flint,       cliopatria(flint),  []).
http:location(api,         cliopatria(api),    []).
http:location(json,        api(json),          []).
http:location(yasgui,      cliopatria(yasgui), []).
http:location(yasqe,       cliopatria(yasqe),  []).
http:location(yasr,        cliopatria(yasr),   []).

                 /*******************************
                 *             TYPES            *
                 *******************************/

%       make the type 'uri' work such  that   we  can  write NS:Local in
%       settings defaults.

:- multifile
    error:has_type/2,
    settings:eval_default/3,
    settings:convert_text/3,
    http_settings:input_item/5.

error:has_type(uri, X) :-               % URI is an atom.  We do not use atom
    atom(X).                        % to allow for conversion

                                        % Convert NS:Local
settings:eval_default(URI, uri, URI) :-
    atom(URI),
    !.
settings:eval_default(NS:Local, uri, URI) :-
    rdf_global_id(NS:Local, URI).

settings:convert_text(uri, Text, URI) :-
    !,
    (   sub_atom(Text, B, _, A, :),
        sub_atom(Text, 0, B, _, NS),
        sub_atom(Text, _, A, 0, Local),
        identifier(NS),
        identifier(Local)
    ->  rdf_global_id(NS:Local, URI)
    ;   URI = Text
    ).

%!  identifier(+Atom) is semidet.
%
%   True if Atom contains only alphanumerical characters or
%   undercores.
%
%   @tbd    Must we support unicode here?  Check with Turtle.

identifier(Text) :-
    atom_codes(Text, Codes),
    identifier_codes(Codes).

identifier_codes([]).
identifier_codes([H|T]) :-
    identifier_code(H),
    identifier_codes(T).

identifier_code(C) :-
    code_type(C, csym).

                                        % Render as plain atom
http_settings:input_item(uri, Value, Name) -->
    html(input([name(Name), size(40), value(Value)])).


                 /*******************************
                 *             HTTP             *
                 *******************************/

:- setting(http:port, any, env('PORT', 3020),
           'Port the http server listens to or interface:port').
:- setting(http:workers, between(1, 20), env('PROLOG_HTTP_WORKERS', 5),
           'Number of server threads').
:- setting(http:worker_options, list(any), [],
           'Additional options to pass to the HTTP server').
:- setting(http:max_idle_time, nonneg, 3600,
           'Session timeout.  If 0, session never times out').
:- setting(http:server_url, atom, 'http://localhost:'+setting(http:port),
           'Url of the server itself').
:- if(\+current_setting(http:prefix)).
:- setting(http:prefix, atom, '',
           'Prefix to rebase the server').
:- endif.


                 /*******************************
                 *      CLIOPATRIA SETTINGS     *
                 *******************************/

:- setting(cliopatria:user_data, atom, 'users.db',
           'File holding account information').
:- setting(cliopatria:enable_self_register, boolean, false,
           'Set to true to allow users to self register'). % using cpa_admin:add_user/1
:- setting(cliopatria:default_entailment, atom, rdfs,
           'Default entailment rules applied').
:- setting(cliopatria:optimise_query, boolean, true,
           'Optimise queries before execution').
:- setting(cliopatria:rdf_db_namespaces, boolean, true,
           'Allow registered namespaces in queries').
:- setting(cliopatria:persistent_store, atom, '',
           'Directory for persistent copy of in-memory RDF').
:- setting(cliopatria:pre_index_tokens, boolean, false,
           'Build the fulltext token index while loading').
:- setting(cliopatria:pre_index_stems, boolean, false,
           'Build the fulltext stem index while loading').


                 /*******************************
                 *        QUERY-SETTINGS        *
                 *******************************/

:- setting(sparql:max_clients, nonneg, 100,
           'Maximum number of concurrent requests').
:- setting(sparql:stack_size, nonneg, 1000,
           'Size of the global stack in mega-bytes').

                 /*******************************
                 *        BROWSE-SETTINGS       *
                 *******************************/

:- setting(cpa_browse:resource_format, atom, nslabel,
           'Default resource_format passed to rdf_link//2').
