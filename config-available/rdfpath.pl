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

:- module(conf_rdfpath, []).

/** <module> Configure location of the RDF (ontology) library

This module uses user:file_search_path/2 to define locations for loading
RDF data. Ontology library directories   contain  files =|Manifest.ttl|=
that describe RDF-data and their dependencies.

Registered bundles of RDF-data that have   a dcterms:title attribute are
displayed on the page  that  appears   from  the  menu-item  =*File/Load
ontology from library*=. Manifests often specify   local files, but they
can also specify RDF from HTTP servers.

@see    rdf_attach_library/1
*/

:- multifile
    user:file_search_path/2.
:- dynamic
    user:file_search_path/2.

/* Assume RDF files are located in a directory
   called 'rdf' located on the same level as the
   directory the server is running from.
*/

user:file_search_path(rdf, '../rdf').
% user:file_search_path(rdf, '../econnect/demo/vocs').
% user:file_search_path(rdf, '../econnect/demo/metadata').


