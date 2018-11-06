/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam
                              Vu University Amsterdam
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

:- module(sparql_xml_result,
          [ sparql_write_xml_result/3   % +Stream, +Result
          ]).
:- use_module(library(sgml)).
:- use_module(library(assoc)).
:- use_module(library(option)).
:- use_module(library('semweb/rdf_db'), [rdf_is_bnode/1, rdf_equal/2]).

ns(sparql, 'http://www.w3.org/2005/sparql-results#').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Read/write   the   SPARQL   XML   result     format    as   defined   in
http://www.w3.org/TR/rdf-sparql-XMLres/, version 6 April 2006.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

                 /*******************************
                 *            WRITING           *
                 *******************************/

%!  sparql_write_xml_result(+Out, +Term, +Options)
%
%   Write SPARQL XML result data.

sparql_write_xml_result(Out, ask(TrueFalse), _Options) :-
    !,
    write_header(Out),
    format(Out, '  <head/>~n', []),
    format(Out, '  <boolean>~w</boolean>~n', [TrueFalse]),
    format(Out, '</sparql>~n', []).
sparql_write_xml_result(Out, update(TrueFalse), Options) :-
    !,
    sparql_write_xml_result(Out, ask(TrueFalse), Options).
sparql_write_xml_result(Out, select(VarTerm, Rows), Options) :-
    VarTerm =.. [_|VarNames],
    option(ordered(Ordered), Options, false),
    option(distinct(Distinct), Options, false),
    write_header(Out),
    format(Out, '  <head>~n', []),
    write_varnames(VarNames, Out),
    format(Out, '  </head>~n', []),
    format(Out, '  <results ordered="~w" distinct="~w">~n',
           [Ordered, Distinct]),
    write_rows(Rows, VarNames, Out),
    format(Out, '  </results>~n', []),
    format(Out, '</sparql>~n', []).

write_header(Out) :-
    xml_encoding(Out, Encoding),
    format(Out, '<?xml version="1.0" encoding="~w"?>~n', [Encoding]),
    ns(sparql, Prefix),
    format(Out, '<sparql xmlns="~w">~n', [Prefix]).

xml_encoding(Out, Encoding) :-
    stream_property(Out, encoding(Enc)),
    (   xml_encoding_name(Enc, Encoding)
    ->  true
    ;   throw(error(domain_error(rdf_encoding, Enc), _))
    ).

xml_encoding_name(ascii,       'US-ASCII').
xml_encoding_name(iso_latin_1, 'ISO-8859-1').
xml_encoding_name(utf8,        'UTF-8').

write_varnames([], _).
write_varnames([H|T], Out) :-
    stream_property(Out, encoding(Encoding)),
    xml_quote_attribute(H, Q, Encoding),
    format(Out, '    <variable name="~w"/>~n', [Q]),
    write_varnames(T, Out).

write_rows(Rows, VarNames, Out) :-
    empty_assoc(BNodes),
    write_rows(Rows, VarNames, Out, state(0, BNodes), _).

write_rows([], _, _, State, State).
write_rows([H|T], VarNames, Out, State0, State) :-
    write_row(H, VarNames, Out, State0, State1),
    write_rows(T, VarNames, Out, State1, State).

write_row(Row, VarNames, Out, State0, State) :-
    format(Out, '    <result>~n', []),
    write_bindings(VarNames, 1, Row, Out, State0, State),
    format(Out, '    </result>~n', []).

write_bindings([], _, _, _, State, State).
write_bindings([Name|T], I, Row, Out, State0, State) :-
    arg(I, Row, Value),
    write_binding(Value, Name, Out, State0, State1),
    I2 is I + 1,
    write_bindings(T, I2, Row, Out, State1, State).

write_binding(Var, _, _, S, S) :- var(Var), !.
write_binding('$null$', _, _, S, S) :- !.
write_binding(Value, Name, Out, State0, State) :-
    stream_property(Out, encoding(Encoding)),
    xml_quote_attribute(Name, Q, Encoding),
    format(Out, '      <binding name="~w">~n', [Q]),
    write_binding_value(Value, Out, State0, State),
    format(Out, '      </binding>~n', []).

write_binding_value(literal(Lit), Out, State, State) :-
    !,
    write_binding_literal(Lit, Out).
write_binding_value(URI, Out, State0, State) :-
    rdf_is_bnode(URI),
    !,
    bnode_id(URI, Id, State0, State),
    format(Out, '        <bnode>~w</bnode>~n', [Id]).
write_binding_value(URI, Out, State, State) :-
    stream_property(Out, encoding(Encoding)),
    xml_quote_cdata(URI, Q, Encoding),
    format(Out, '        <uri>~w</uri>~n', [Q]).

%!  write_binding_literal(+Literal, +Out) is det.
%
%   Write Literal to Out. The  first   clause  deals with XMLLiteral
%   fields. The SPARQL documentation is rather  vaque about how this
%   should be handled. It might well be that we should write the xml
%   into an atom and xml-escape that.

write_binding_literal(type(Type, DOM), Out) :-
    rdf_equal(Type, rdf:'XMLLiteral'),
    xml_is_dom(DOM),
    !,
    with_output_to(string(S),
                   xml_write(current_output, DOM,
                             [ header(false),
                               layout(false)
                             ])),
    stream_property(Out, encoding(Encoding)),
    xml_quote_cdata(S, QV, Encoding),
    format(Out, '        <literal datatype="~w">~w</literal>~n', [Type, QV]).
write_binding_literal(type(Type, Value), Out) :-
    !,
    stream_property(Out, encoding(Encoding)),
    xml_quote_attribute(Type, QT, Encoding),
    (   atom(Value)
    ->  xml_quote_cdata(Value, QV, Encoding)
    ;   QV = Value                  % ok for numbers, etc.
    ),
    format(Out, '        <literal datatype="~w">~w</literal>~n', [QT, QV]).
write_binding_literal(lang(L, Value), Out) :-
    !,
    stream_property(Out, encoding(Encoding)),
    xml_quote_cdata(Value, QV, Encoding),
    format(Out, '        <literal xml:lang="~w">~w</literal>~n', [L, QV]).
write_binding_literal(Value, Out) :-
    !,
    stream_property(Out, encoding(Encoding)),
    xml_quote_cdata(Value, QV, Encoding),
    format(Out, '        <literal>~w</literal>~n', [QV]).

bnode_id(URI, Id, State0, State) :-
    State0 = state(N, Assoc),
    (   get_assoc(URI, Assoc, Id)
    ->  State = State0
    ;   N2 is N + 1,
        Id = N2,                    % number 1, ...
        put_assoc(URI, Assoc, Id, Assoc2),
        State = state(N2, Assoc2)
    ).
