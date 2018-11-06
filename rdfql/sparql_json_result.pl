/*  Part of ClioPatria SeRQL and SPARQL server

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

:- module(sparql_json_result,
          [ sparql_write_json_result/3  % +Out, +Result, +Options
          ]).
:- use_module(library(http/http_json)).
:- use_module(library(sgml_write)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).

/** <module> Write SPARQL results as JSON

@tbd    Support other SPARQL request results
@author Jan Wielemaker
@author Michiel Hildebrand
*/

sparql_json_mime_type(application/'sparql-results+json; charset=UTF-8').

%!  sparql_write_json_result(+Out:stream, +Result, +Options) is det.
%
%   Emit results from a SPARQL query as JSON.
%
%   @see http://www.w3.org/TR/rdf-sparql-json-res/

sparql_write_json_result(Out, select(VarTerm, Rows), Options) :-
    VarTerm =.. [_|VarNames],
    JSON = json([ head    = json([vars=VarNames]),
                  results = json([bindings=Bindings])
                ]),
    maplist(row_to_json(VarNames), Rows, Bindings),
    (   option(content_type(_), Options)
    ->  JSONOptions = Options
    ;   sparql_json_mime_type(Mime),
        JSONOptions = [content_type(Mime)|Options]
    ),
    with_output_to(Out, reply_json(JSON, JSONOptions)).
sparql_write_json_result(Out, ask(True), Options) :-
    JSON = json([ head    = json([]),
                  boolean = @(True)
                ]),
    (   option(content_type(_), Options)
    ->  JSONOptions = Options
    ;   sparql_json_mime_type(Mime),
        JSONOptions = [content_type(Mime)|Options]
    ),
    with_output_to(Out, reply_json(JSON, JSONOptions)).


row_to_json(Vars, Row, json(Bindings)) :-
    var_col_bindings(Vars, 1, Row, Bindings).

var_col_bindings([], _, _, []).
var_col_bindings([V0|T0], I, Row, Bindings) :-
    arg(I, Row, Value),
    I2 is I + 1,
    (   Value = '$null$'            % also catches variables
    ->  var_col_bindings(T0, I2, Row, Bindings)
    ;   Bindings = [V0=json(JSON)|T],
        rdf_term_to_json(Value, JSON),
        var_col_bindings(T0, I2, Row, T)
    ).


%!  rdf_term_to_json(+RDFTerm, -JsonTerm)
%
%   convert an rdf term to a json term.
%
%   @tbd: Handle blank nodes.

rdf_term_to_json(literal(Lit), Object) :-
    !,
    Object = [type=literal, value=Txt|Rest],
    literal_to_json(Lit, Txt, Rest).
rdf_term_to_json(URI0, Object) :-
    rdf_global_id(URI0, URI),
    Object = [type=Type, value=URI],
    object_uri_type(URI, Type).

%!  literal_to_json(+Literal, -Text, -Attributes)
%
%   Extract text and Attributes from Literal resource.

literal_to_json(lang(Lang, Text), Text, ['xml:lang'=Lang]) :- !.
literal_to_json(type(Type, Text0), Text, [datatype=Type]) :-
    !,
    to_text(Type, Text0, Text).
literal_to_json(Txt, Txt, []).

to_text(_Type, Text, Text) :-
    atomic(Text).
to_text(Type, DOM, Text) :-
    rdf_equal(Type, rdf:'XMLLiteral'),
    !,
    with_output_to(string(Text),
                   xml_write(DOM, [header(false)])),
    atomic(Text).

%!  object_uri_type(+URI, -Type)
%
%   Type is one of bnode or uri.

object_uri_type(URI, Type) :-
    (   rdf_is_bnode(URI)
    ->  Type = bnode
    ;   Type = uri
    ).

                 /*******************************
                 *   INTERACTIVE QUERY RESULT   *
                 *******************************/

:- multifile
    rdf_io:write_table/4.

rdf_io:write_table(json, _, Rows, Options) :-
    memberchk(variables(Vars), Options),
    !,
    (   is_list(Vars)
    ->  VarTerm =.. [vars|Vars]
    ;   VarTerm = Vars
    ),
    sparql_write_json_result(current_output, select(VarTerm, Rows),
                             [ content_type(text/plain),
                               Options
                             ]).
