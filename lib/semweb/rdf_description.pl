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

:- module(rdf_description,
          [ description_property/1,     % ?Property
            rdf_description/2           % +Resource, ?Description
          ]).
:- use_module(library(semweb/rdf_db)).

/** <module> Deal with descriptive nodes in RDF models

@tbd    Define sensible operations on description properties, similar
        to rdf_label.pl.
@tbd    Language selection
*/

:- rdf_meta
    description_property(r).
:- multifile
    description_property/1,
    description_hook/2.

%!  description_property(?Property)
%
%   True when Property is used for   descritive nodes on a resource.
%   This predicate is multifile.

description_property(dc:description).
description_property(skos:scopeNote).
description_property(rdfs:comment).


%!  rdf_description(+R, -Description:literal) is nondet.
%
%   Description is a description for R.   This predicate first calls
%   the hook description_hook/2. If this hook  fails it produces all
%   property-values    for    the     properties      defined     by
%   description_property/1 that have a literal value.

rdf_description(R, Description) :-
    (   description_hook(R, Description)
    *-> true
    ;   description_property(P),
        rdf_has(R, P, Description),
        rdf_is_literal(Description)
    ).
