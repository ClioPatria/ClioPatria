/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam
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

:- module(rdf_io,
          [ write_table/2,      % +Row, +Options
            write_graph/2,      % +Triples, +Options
            get_triples/3       % +Input, -Triples, +Options
          ]).
:- use_module(library('semweb/rdf_db')).
:- use_module(library(rdf_write)).
:- use_module(library(rdf)).
:- use_module(library(lists)).

:- multifile
    write_table/4,          % +Format, +Serialization, +Rows, +Options
    write_graph/4,          % +Format, +Serialization, +Triples, +Options
    get_triples/4.          % +Format, +Input, -Triples, +Options

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module acts as a dispatcher module,   allowing other modules to add
clauses  for  write_table/4  and  write_graph/4    and   thus  providing
additional output formats without modifications to the kernel source.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%!  write_table(+Rows, +Options)
%
%   Write a result-table in the specified format.  Rows is a list of
%   terms row(C1, C2, ...).  Options specifies additional processing
%   options.  Defined options are:
%
%       * result_format(+Format)
%       Specifies the output format.  Defined formats depend on
%       the loaded plugins. Passed as first argument to the
%       write_table/4 hook.  This option *must* be present.
%
%       * serialization(+Serialization)
%       Specifies the serialization of the output. Passed as second
%       argument to the write_table/4 hook.  This option *must* be present.
%
%       * variables(+Vars)
%       Specifies the names of the columns.  Vars is a term with
%       functor =vars= and atom-arguments describing the names
%       of each subsequent column.  For Example:
%
%           ==
%           variables(vars('Name', 'Address'))
%           ==
%
%   The output hooks may support additional options.
%
%   @param Rows is a list of terms row(Col1, Col2, ..., ColN)


write_table(Rows, Options) :-
    needed_option(result_format(Format), Options),
    needed_option(serialization(Serialization), Options),
    write_table(Format, Serialization, Rows, Options).

%!  write_graph(+Triples, +Options)
%
%   Write a graph, represented as a list of rdf(S,P,O) triples.
%   Options:
%
%       * result_format(+Format)
%       Specifies the output format.  Defined formats depend on
%       the loaded plugins. Passed as first argument to the
%       write_graph/4 hook.
%
%       * serialization(+Serialization)
%       Specifies the serialization of the output. Passed as second
%       argument to the write_table/4 hook.  This option *must* be present.

write_graph(Triples, Options) :-
    needed_option(serialization(Serialization), Options),
    (   Serialization == rdfxml
    ->  (   memberchk(result_format(Format), Options)
        ->  true
        ;   Format = xml
        )
    ;   option(result_format(Format), Options)
    ->  true
    ;   Format = Serialization
    ),
    write_graph(Format, Serialization, Triples, Options).


                 /*******************************
                 *             READING          *
                 *******************************/

%!  get_triples(+Input, -Graph:list, +Options)
%
%   Read triples according to the option data_format. This predicate
%   is plugable by get_triples/4.

get_triples(Input, Triples, Options0) :-
    select(data_format(Format), Options0, Options),
    get_triples(Format, Input, Triples, Options).

%!  get_triples(+Format, +Input, -Graph:list, +Options)
%
%   Hook to read triples into  a  list   of  rdf(S,P,O)  for a given
%   Format. The default implementation supports =rdfxml= by means of
%   load_rdf/3.

get_triples(rdfxml, Input, Triples, Options) :-
    !,
    load_rdf(Input, Triples, Options).


                 /*******************************
                 *             HOOK             *
                 *******************************/

%!  write_table(+ResultFormat, +Serialization, +Triples, +Options)
%
%   Hook for write_table/2.  There is no default implementation.

%!  write_graph(+ResultFormat, +Serialization, +Triples, +Options)
%
%   Hook for write_graph/2.  The   default  implementation  supports
%   Format  =  =xml=  and   Serialization    =   =rdfxml=.  It  uses
%   rdf_write_xml/2 to emit the graph.

write_graph(xml, rdfxml, Triples, Options) :-
    option(mimetype(Type), Options, 'application/rdf+xml'),
    format('Transfer-encoding: chunked~n'),
    format('Content-type: ~w; charset=UTF-8~n~n', [Type]),
    rdf_write_xml(current_output, Triples).


                 /*******************************
                 *              UTIL            *
                 *******************************/

needed_option(Term, Options) :-
    memberchk(Term, Options),
    !.
needed_option(Term, _) :-
    functor(Term, Name, _),
    throw(error(existence_error(option, Name), _)).
