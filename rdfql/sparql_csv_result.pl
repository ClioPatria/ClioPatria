/*  Part of ClioPatria

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2018, VU University Amsterdam
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

:- module(sparql_csv_result,
          [ sparql_write_csv_result/3   % +Out, +Result, +Options
          ]).
:- use_module(library(csv)).
:- use_module(library(assoc)).
:- use_module(library(option)).
:- use_module(library(sgml_write)).
:- use_module(library(semweb/rdf_db)).
:- if(exists_source(library(semweb/rdf11))).
:- use_module(library(semweb/rdf11),
              [ rdf_lexical_form/2
              ]).
:- endif.

/** <module> Write SPARQL results as CSV

@see    http://www.w3.org/TR/2013/REC-sparql11-results-csv-tsv-20130321/
*/

sparql_csv_mime_type('text/tab-separated-values; charset=UTF-8').

%!  sparql_write_csv_result(+Out:stream, +Result, +Options) is det.
%
%   Emit results from a SPARQL SELECT query as CSV.  Options:
%
%     - bnode_state(State0-State)
%     Maintain blank node mapping accross multiple calls.  State0
%     is either a variable or a state returned by a previous call.
%     - http_header(+Boolean)
%     if `true` (default), emit an HTTP =Content-type= header.
%     - header_row(+Boolean)
%     if `true` (default), emit header row with binding names.
%
%   @see http://www.w3.org/TR/rdf-sparql-json-res/

sparql_write_csv_result(Out, select(VarTerm, Rows), Options) :-
    !,
    option(bnode_state(BNodes0-BNodes), Options, _),
    (   var(BNodes0)
    ->  empty_assoc(BNodeDict),
        BNodes0 = bnode(1, BNodeDict)
    ;   true
    ),
    rows_to_csv(Rows, CSVRows, BNodes0, BNodes),
    (   option(http_header(true), Options, true)
    ->  sparql_csv_mime_type(ContentType),
        format('Content-type: ~w~n~n', [ContentType])
    ;   true
    ),
    (   option(header_row(true), Options, true)
    ->  csv_write_stream(Out, [VarTerm|CSVRows], [])
    ;   csv_write_stream(Out, CSVRows, [])
    ).
sparql_write_csv_result(_Out, Result, _Options) :-
    !,
    domain_error(csv_sparql_result, Result).

rows_to_csv([], [], BNodeDict, BNodeDict).
rows_to_csv([H0|T0], [H|T], BNodeDict0, BNodeDict) :-
    row_to_csv(H0, H, BNodeDict0, BNodeDict1),
    rows_to_csv(T0, T, BNodeDict1, BNodeDict).

row_to_csv(RDF, CSV, BNodeDict0, BNodeDict) :-
    RDF =.. [_|RDFFields],
    fields_to_csv(RDFFields, CSVFields, BNodeDict0, BNodeDict),
    CSV =.. [row|CSVFields].

fields_to_csv([], [], BNodeDict, BNodeDict).
fields_to_csv([H0|T0], [H|T], BNodeDict0, BNodeDict) :-
    field_to_csv(H0, H, BNodeDict0, BNodeDict1),
    fields_to_csv(T0, T, BNodeDict1, BNodeDict).

field_to_csv(Var, '', BNodeDict, BNodeDict) :-
    (   var(Var)
    ->  true
    ;   Var == '$null$'
    ),
    !.
field_to_csv(literal(Literal), Text, BNodeDict, BNodeDict) :-
    literal_text(Literal, Text),
    !.
field_to_csv(@(LangString,Lang), Text, BNodeDict, BNodeDict) :-
    literal_text(@(LangString,Lang), Text),
    !.
field_to_csv(^^(Lexical,Type), Text, BNodeDict, BNodeDict) :-
    literal_text(^^(Lexical,Type), Text),
    !.
field_to_csv(Resource, BNode, BNodeDict0, BNodeDict) :-
    rdf_is_bnode(Resource),
    !,
    BNodeDict0 = bnode(N0, Dict0),
    (   get_assoc(Resource, Dict0, BNode)
    ->  BNodeDict = BNodeDict0
    ;   succ(N0, N),
        atomic_list_concat(['_:node', N], BNode),
        put_assoc(Resource, Dict0, BNode, Dict),
        BNodeDict = bnode(N, Dict)
    ).
field_to_csv(Atomic, Atomic, BNodeDict, BNodeDict) :-
    atomic(Atomic),
    !.
field_to_csv(Term, String, BNodeDict, BNodeDict) :-
    term_string(Term, String).

%!  literal_text(+Literal, -Value) is semidet.

literal_text(type(Type, Value), Text) :-
    !,
    atom(Type),
    (   rdf_equal(Type, rdf:'XMLLiteral')
    ->  with_output_to(string(Text),
                       xml_write(Value, [header(false)]))
    ;   atomic(Value)
    ->  Text = Value
    ;   term_string(Value, Text)
    ).
literal_text(lang(Lang, LangText), Text) :-
    !,
    atom(Lang),
    literal_text(LangText, Text).
literal_text(@(LangText, Lang), Text) :-
    !,
    atom(Lang),
    literal_text(LangText, Text).
:- if(current_predicate(rdf_lexical_form/2)).
literal_text(^^(Lexical, Type), Text) :-
    !,
    rdf_lexical_form(^^(Lexical,Type), ^^(Text,_)).
:- endif.
literal_text(Text, Text) :-
    atom(Text),
    !.
literal_text(Text, Text) :-
    string(Text),
    !.


                 /*******************************
                 *   INTERACTIVE QUERY RESULT   *
                 *******************************/

:- multifile
    rdf_io:write_table/4.

rdf_io:write_table(csv, _, Rows, Options) :-
    memberchk(variables(Vars), Options),
    !,
    (   is_list(Vars)
    ->  VarTerm =.. [vars|Vars]
    ;   VarTerm = Vars
    ),
    sparql_write_csv_result(current_output, select(VarTerm, Rows),
                            [ content_type(text/plain),
                              Options
                            ]).
