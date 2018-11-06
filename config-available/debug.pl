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

:- module(conf_debug, [ tmon/0 ]).
:- use_module(library(debug)).

/** <module> Set options for development

This  module  make  ClioPatria  more    suitable   for  development.  In
particular, it implements the following methods:

    * Load library(http/http_error), which causes uncaught Prolog
    exceptions to produce an HTML page holding a stack-trace.
    * Load library(semweb/rdf_portray), which prints RDF resources
    in a more compact way.
    * Load library(semweb/rdf_db) into the module =user=.  This allows
    usage of rdf/3, rdf_load/1, etc. from the toplevel without
    specifying the module.
    * Use debug/1 on the _topic_ http(request), which causes the
    toplevel to print basic information on the HTTP requests processed.
    Using copy/paste of the HTTP path, one can assemble a command that
    edits the implementation of a page.

        ==
        ?- edit('/http/path/to/handler').
        ==
    * Define tmon/0 that brings up a graphical tool showing thread
    activity.

@see    http://www.swi-prolog.org/howto/http/Developing.html
*/

:- use_module(library(http/http_error)).        % Print stack on error
:- use_module(library(semweb/rdf_portray)).     % Print e.g., rdf:type
:- use_module(user:library(semweb/rdf_db)).     % Allow ?- rdf(S,P,O). in toplevel

:- debug_message_context(+time).                % Add time to debug message
% Enable to see HTTP requests
% :- debug(http(request)).                      % Print request and reply

%!  prepare_editor
%
%   Start XPCE as edit requests comming from the document server can
%   only be handled if XPCE is running.

prepare_editor :-
    current_prolog_flag(editor, pce_emacs),
    !,
    start_emacs.
prepare_editor.

:- prepare_editor.

%!  tmon
%
%   Show the graphical thread-monitor. Can be  useful to examine and
%   debug active goals.

tmon :-
    call(prolog_ide(thread_monitor)).
