/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2018, University of Amsterdam
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

:- module(rdf_turtle_io,
          [
          ]).
:- use_module(library(semweb/rdf_turtle_write)).

:- multifile
    rdf_io:write_graph/4.

/** <module> Write query-result graphs as Turtle

This module writes a SPARQL graph results   using  the Turtle format. It
acts as a hook into rdf_io.pl.
*/

                 /*******************************
                 *          GRAPH OUTPUT        *
                 *******************************/

%!  rdf_io:write_graph(+Format, +Serialization, +Triples, +Options)
%
%   Write an RDF result-graph as an  HTML table, where resources are
%   links to the ClioPatria local view.
%
%   @param Format is one of =turtle= or =canonical_turtle=
%   @param Serialization is ignored
%   @param Triples is a list of rdf(S,P,O) triples

rdf_io:write_graph(turtle, _Serialization, Triples, Options) :-
    option(mimetype(Type), Options, 'text/turtle'),
    format('Transfer-encoding: chunked~n'),
    format('Content-type: ~w~n~n', [Type]),
    (   Triples == []
    ->  format('# Graph contains no data~n', [])
    ;   rdf_save_turtle(stream(current_output),
                        [ expand(triple_in(Triples))
                        ])
    ).
rdf_io:write_graph(canonical_turtle, _Serialization, Triples, Options) :-
    option(mimetype(Type), Options, 'text/turtle'),
    format('Transfer-encoding: chunked~n'),
    format('Content-type: ~w~n~n', [Type]),
    (   Triples == []
    ->  format('# Graph contains no data~n', [])
    ;   rdf_save_canonical_turtle(stream(current_output),
                                  [ expand(triple_in(Triples))
                                  ])
    ).

:- public
    triple_in/5.

triple_in(RDF, S,P,O,_G) :-
    member(rdf(S,P,O), RDF).
