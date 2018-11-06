/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam,
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

:- module(bundle,
          [ use_bundle/1
          ]).

/** <module> Import bundles

This module provides an interface that is   similar  to the Ciao package
interface. Bundles are Prolog files that   are included. Typically, they
contain multiple use_module/1 calls.

At least for now, the system  has   been  named  bundle because the name
package is already used as a package of software.
*/

:- multifile
    user:file_search_path/2,
    system:term_expansion/2,
    emacs_prolog_colours:goal_colours/2.

user:file_search_path(bundle,       library(bundles)).

%!  use_bundle(+Term)
%
%   Include a bundle of declarations.  Typically, these declarations
%   are a set of :- use_module(Library). calls.

use_bundle(Package) :-
    throw(error(context_error(nodirective, use_bundle(Package)), _)).

system:term_expansion((:- use_bundle(Package)),
                      (:- include(bundle(Package)))).


emacs_prolog_colours:goal_colours(use_bundle(Pkg),
                                  built_in - [ Class ]) :-
    (   absolute_file_name(bundle(Pkg), File,
                           [ file_type(prolog),
                             file_errors(fail)
                           ])
    ->  Class = file(File)
    ;   Class = nofile
    ).
