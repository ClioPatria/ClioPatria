/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
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

:- module(test_sparql,
          [ sparql_parse/3,             % +Text, -Query, +Options

            load_manifests/1,           % 'arq', 'dawg' or 'sparql11'
            load_test_data/1,           % +NameOrIRI

            show_test/1,                % +NameOrIRI
            show_test_data/1,           % +NameOrIRI
            edit_test_data/1,           % +NameOrIRI
            edit_test_result/1,         % +NameOrIRI
            edit_test/1,                % +NameOrIRI

            list_db/0,

            list_tests/1,               % +Class
            dump_tests/1,               % +File
            compare_tests/1,            % +File

                                        % SYNTAX TESTS
            syntax_test/1,              % +NameOrIRI
            syntax_test/2,              % +NameOrIRI, -Query
            run_syntax_tests/0,
            test_query_listing/0,
            test_query_listing/1,       % +NameOrIRI

                                        % QUERY TESTS
            run_query_tests/0,
            query_test/1                % +NameOrIRI
          ]).

user:file_search_path(rdfql, '../rdfql').
user:file_search_path(entailment, '../entailment').
user:file_search_path(library, '../lib').

:- use_module(library(memfile)).
:- use_module(rdfql(sparql_grammar)).
:- use_module(rdfql(sparql)).
:- use_module(rdfql(jena_functions)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/sparql_client),
              [ sparql_read_xml_result/2,
                sparql_read_json_result/2
              ]).
:- use_module(library(apply)).
:- use_module(library(settings)).
:- use_module(test_manifest).
:- use_module(entailment(rdf), []).
:- use_module(entailment(none), []).
                                        % Toplevel debugging utilities
:- use_module(user:library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle_write)).

:- setting(cliopatria:optimise_query, boolean, true,
           'Optimise queries before execution').

/** <module> SPARQL test suite handling

@see http://sparql.org/query-validator.html
*/

:- dynamic
    failed_result/2,
    passed/1,
    failed/1,
    skipped/2.

%!  blocked(?Name)
%
%   Blocked tests

:- multifile
    blocked/1.

blocked('extendedType-literal-ne').
blocked('typePromotion-decimal-decimal-pass').
blocked('extendedType-ne-fail').
                                        % ARQ tests with .srj result file
blocked('strlen - 1').                  % is this JSON?
blocked('strlen - 2').
                                        % ARQ tests with UNSAID
blocked('One optional clause (alt)').
blocked('Two optional clauses (alt)').
blocked('UNSAID - triple/present').
blocked('UNSAID - triple/absent').
blocked('UNSAID of pattern in basic block => false').
blocked('UNSAID of pattern not matching').
blocked('UNSAID of pattern partially matching').
                                        % Broken tests (as far as I can tell)
blocked('Union 6').
                                        % test with property functions with
                                        % two arguments
blocked(Test) :-
    sub_atom(Test, _, _, _, splitIRI),
    !.

% SPARQL 1.1
blocked('csv03 - CSV Result Format').   % Too bizarre types


                 /*******************************
                 *           QUERY TESTS        *
                 *******************************/

%       TBD: As we have to have an  empty database we must first collect
%       all info in the Prolog database :-(  Alternatively we need a way
%       to set the current database (thread-local!)

%       run_query_tests
%
%       Load all manifests and execute the tests.

run_query_tests :-
    clean_tests,
    load_query_manifests,
    run_all_query_tests.

load_query_manifests :-
    load_manifests([ arq,
                     dawg,
                     sparql11
                   ]).

run_all_query_tests :-
    (   current_test(_, Test),
        test_name(Test, Name),
        (   blocked(Name)
        ->  assert(skipped(Test, blocked)),
            fail
        ;   true
        ),
        query_test(Test),
        fail ; true
    ),
    test_statistics.

query_test(Name) :-
    test_name(Test, Name),
    !,
    query_test(Test).
query_test(Test) :-
    test_syntax(Test, Syntax),
    (   Syntax == negative
    ->  assertz(skipped(Test, negsyntax))
    ;   Syntax == positive
    ->  assertz(skipped(Test, syntax))
    ),
    !.
query_test(Test) :-
    (   test_query(Test, Query)
    ->  (   entailment(Test, Entailment)
        ->  query_test(Test, Query, Entailment)
        ;   assertz(skipped(Test, entailment))
        )
    ;   assertz(skipped(Test, no_query))
    ).


query_test(Test, Query, Entailment) :-
    test_name(Test, Name),
    format('~`=t BEGIN ~q ~`=t~72|~n', [Name]),
                                    % Compile the query
    (   catch(sparql_compile(Query, Compiled,
                             [ type(Type),
                               entailment(none),
                               bind_null(true),
                               entailment(Entailment)
                             ]), E, true)
    ->  (   var(E)
        ->  true
        ;   print_message(error, E),
            assert(failed(Test)),
            fail
        )
    ;   test_id(Test, Id),
        format('FAILED to compile ~q~n', [Id]),
        assert(failed(Test)),
        fail
    ),
                                    % get the correct result
    result_to_prolog(Type, Test, PrologResult),
    load_test_data(Test),
                                    % run the query
    catch(findall(Result, sparql_run(Compiled, Result), Results),
          E, true),
    (   var(E)
    ->                              % compare the result
        compare_results(Test, Type, PrologResult, Results)
    ;   print_message(error, E),
        assert(failed(Test)),
        fail
    ).

entailment(Test, Entailment) :-
    test_entailment(Test, EntailURI),
    !,
    entailment_name(EntailURI, Entailment).
entailment(_, none).

:- rdf_meta entailment_name(r, -).

entailment_name(entailment:'RDF',        rdf).
entailment_name(entailment:'RDFS',       rdfs).
entailment_name(entailment:'OWL-Direct', owl_direct) :- fail.


%!  load_test_data(+Test)
%
%   Reset the RDF store and load  all   data  into the desired named
%   graphs.

load_test_data(Name) :-
    test_name(Test, Name),
    !,
    load_test_data(Test).
load_test_data(Test) :-
    rdf_reset_db,
    test_data_files(Test, DataFiles),
    !,
    forall(member(File-Graph, DataFiles),
           rdf_load(File, [graph(Graph)])).
load_test_data(_) :-
    print_message(informational, sparql(no_test_data)).


%!  compare_results(+Test, +Type, +Correct, +Results)
%
%   NOTE: Some tests (notably syntax tests)  do not have results. In
%   that case Results is bound to no_result and we do not compare.

compare_results(_, _, no_result, _) :- !.
compare_results(Test, ask, ask(Bool), [Bool]) :-
    !,
    assert(passed(Test)).
compare_results(Test, Type, select(ColTerm, Rows), Result) :-
    exclude(ground, Result, NonGround),
    (   NonGround \== []
    ->  write_list('NON GROUND:', NonGround, 8),
        assert(failed(Test))
    ;   true
    ),
    Type = select(MyColTerm),
    !,
    MyColTerm =.. [_|MyColNames],
    ColTerm =.. [_|ColNames],
    same_colnames(ColNames, MyColNames, Map),
    (   Map == nomap
    ->  RowsMyOrder = Rows
    ;   map_rows(Rows, Map, RowsMyOrder)
    ),
    compare_sets(Test, Type, RowsMyOrder, Result).
compare_results(Test, construct, Correct, Result) :-
    !,
    compare_sets(Test, compare_sets, Correct, Result).
compare_results(Test, describe, Correct, Result) :-
    !,
    compare_sets(Test, compare_sets, Correct, Result).
compare_results(Test, ask, ask(Correct), [Result]) :-
    !,
    test_id(Test, Name),
    format('~`=t ~q ~`=t~72|~n', [Name]),
    format('TYPE: ASK~n'),
    format('CORRECT: ~q~n', [Correct]),
    format('WE: ~q~n', [Result]).
compare_results(Test, update, Graphs, Result) :-
    !,
    compare_update(Test, Graphs, Result).
compare_results(Test, Type, Correct, Result) :-
    test_id(Test, Name),
    format('~`=t ~q ~`=t~72|~n', [Name]),
    format('TYPE: ~q~n', [Type]),
    write_list('CORRECT:', Correct, 8),
    write_list('WE:', Result, 8),
    format('~`=t~72|~n~n', []),
    assert(failed(Test)).

compare_sets(Test, Type, RowsMyOrder, Result) :-
    var_blank_nodes_in_rows(RowsMyOrder, RowsMyOrderV),
    var_blank_nodes_in_rows(Result, ResultV),
    sort(RowsMyOrderV, OkRows),             % TBD: match blank nodes!
    sort(ResultV, MyRows),
    match_rows(MyRows, OkRows, MyExtra, OkExtra),
    !,
    (   MyExtra == [],
        OkExtra == []
    ->  assert(passed(Test))
    ;   test_id(Test, Name),
        length(MyRows, MyCount),
        length(MyExtra, MyExtraCount),
        length(OkExtra, OkExtraCount),
        format('~`=t ~q ~`=t~72|~n', [Name]),
        format('TYPE: ~q~n', [Type]),
        format('RESULTS: ~D; ~D missed, ~D incorrect~n', [MyCount, OkExtraCount, MyExtraCount]),
        write_list('MISSED:', OkExtra, 8),
        write_list('EXTRA:', MyExtra, 8),
        format('~`=t~72|~n~n', []),
        assert(failed(Test))
    ).


%!  same_colnames(+Names1, +Names2, -Map)
%
%   Is true if Names1 and Names2 contain  the same names in possibly
%   different order. If the order is diffent,   Map  is unified to a
%   term map(RowLeft, RowRight), where variable bindings between the
%   row-terms express the mapping between the rows.

same_colnames(Names, Names, nomap) :- !.
same_colnames(Names1, Names2, map(R1,R2)) :-
    msort(Names1, S),
    msort(Names2, S),
    length(Names1, Len),
    functor(R1, row, Len),
    functor(R2, row, Len),
    fill_vars(Names1, 1, Names2, R1, R2).

fill_vars([], _, _, _, _).
fill_vars([H|T], I, Names, R1, R2) :-
    arg(I, R1, V),
    nth1(I2, Names, H),
    !,
    arg(I2, R2, V),
    IN is I + 1,
    fill_vars(T, IN, Names, R1, R2).

map_rows([], _, []).
map_rows([H0|T0], Map, [H|T]) :-
    copy_term(Map, map(H0, H)),
    map_rows(T0, Map, T).


%!  var_blank_nodes_in_rows(+Rows0, -Rows)
%
%   Substitute blank nodes in rows by variables.  Note that blank
%   nodes of multiple rows are not related.

var_blank_nodes_in_rows([], []).
var_blank_nodes_in_rows([H0|T0], [H|T]) :-
    var_blank_nodes(H0, H),
    var_blank_nodes(T0, T).


%!  var_blank_nodes(+Term, -VarBlanks)
%
%   Process Term, replacing blank nodes with variables.

var_blank_nodes(Term0, Term) :-
    empty_assoc(Vars),
    var_blank_nodes(Term0, Vars, _, Term).

var_blank_nodes(BN, Vars0, Vars, V) :-
    rdf_is_bnode(BN),
    !,
    (   get_assoc(BN, Vars0, V)
    ->  Vars = Vars0
    ;   put_assoc(BN, Vars0, V, Vars)
    ).
var_blank_nodes(Term0, Vars0, Vars, Term) :-
    compound(Term0),
    !,
    functor(Term0, Name, Arity),
    functor(Term,  Name, Arity),
    var_blank_nodes_args(0, Arity, Term0, Vars0, Vars, Term).
var_blank_nodes(Term, Vars, Vars, Term).

var_blank_nodes_args(I, I, _, Vars, Vars, _) :- !.
var_blank_nodes_args(I0, Arity, Term0, Vars0, Vars, Term) :-
    I is I0 + 1,
    arg(I, Term0, A0),
    var_blank_nodes(A0, Vars0, Vars1, A),
    arg(I, Term, A),
    var_blank_nodes_args(I, Arity, Term0, Vars1, Vars, Term).


%!  match_rows(+Rows0, +Rows1, -Extra1, -Extra2)
%
%   Succeed if both sets of rows are   the same. Note that there may
%   be blank nodes in the  rows.   These  are already substituted by
%   Prolog variables. This is basically a permutation problem. First
%   we decide to which row each row can  match. Then we sort them to
%   the  lowest  number  of  matches  and    finally  we  start  the
%   non-deterministic matching process.

match_rows(Rows1, Rows2, Extra1, Extra2) :-
    candidate_matches(Rows1, Rows2, Extra1, Extra1Tail, Candidates),
    keysort(Candidates, Sorted),
    same_rows(Sorted, Rows2, Extra1Tail, Extra2).

candidate_matches([], _, E, E, []).
candidate_matches([H0|T0], Rows, E0, E, [L-Rs|T]) :-
    setof(R, member_row(H0, R, Rows), Rs),
    !,
    length(Rs, L),
    candidate_matches(T0, Rows, E0, E, T).
candidate_matches([H0|T0], Rows, [H0|E0], E, T) :-
    candidate_matches(T0, Rows, E0, E, T).

same_rows(E, [], E, []) :- !.
same_rows([], E, [], E) :- !.
same_rows([_-Rs|T], Rows, E1, E2) :-
    member(R, Rs),
    select_eq(R, Rows, Rest),
    same_rows(T, Rest, E1, E2),
    !.

select_eq(V, Set, Rest) :-
    select(X, Set, Rest),
    V == X.

member_row(R, R1, Rows) :-
    member(R1, Rows),
    same_row(R, R1).

same_row(R, R) :- !.
same_row(R1, R2) :-
    forall(arg(I, R1, A1),
           (   arg(I, R2, A2),
               same_value(A1, A2))).

same_value(V, V) :- !.
same_value(literal(L1), literal(L2)) :-
    same_literal(L1, L2).

:- rdf_meta same_literal(t,t).

same_literal(lang(L1,S), lang(L2,S)) :-
    downcase_atom(L1, L),
    downcase_atom(L2, L).
same_literal(type(xsd:string, X), X).
same_literal(X, type(xsd:string, X)).
same_literal(type(T,V1), type(T,V2)) :-
    xsdp_numeric_uri(T, _),
    to_number(V1, N1),
    to_number(V2, N2),
    (   N1 =:= N2
    ->  true
    ;   float(N1), float(N2),
        abs(N1-N2) < 0.000000001    % hack for rounding errors.
    ).

to_number(a, _) :- !, fail.             % catch variables
to_number(N, N) :-
    number(N),
    !.
to_number(A, N) :-
    atom_number(A, N).


%!  write_list(+Prompt, +List, +Indent)
%
%   Write list of rows or rdf terms.

write_list(_, [], _) :- !.
write_list(Prompt, List, Indent) :-
    !,
    format('~w', [Prompt]),
    write_list2(List, Indent).

write_list2([], _).
write_list2([H|T], Indent) :-
    format('~t~*|', [Indent]),
    (   H =.. [row|Cols]
    ->  write_cols(Cols),
        format(' .~n')
    ;   H = rdf(S,P,O)
    ->  write_cell(S), put(' '),
        write_cell(P), put(' '),
        write_cell(O), write(' .\n')
    ;   format('~p~n', [H])
    ),
    write_list2(T, Indent).


write_cols([]).
write_cols([H|T]) :-
    write_cell(H),
    (   T == []
    ->  true
    ;   put(' '),
        write_cols(T)
    ).

write_cell(Var) :-
    var(Var),
    !,
    format('~w', [Var]).
write_cell(literal(type(T,V))) :-
    !,
    (   atom(T),
        rdf_global_id(NS:L, T)
    ->  format('"~w"^^~w:~w', [V, NS, L])
    ;   format('"~w"^^<~w>', [V, T])
    ).
write_cell(literal(lang(L,V))) :-
    !,
    format('"~w"@~w', [V, L]).
write_cell(literal(V)) :-
    !,
    format('"~w"', [V]).
write_cell(R) :-
    atom(R),
    rdf_global_id(NS:Id, R),
    !,
    format('~w:~w', [NS, Id]).
write_cell('$null$') :-
    !,
    write('NULL').
write_cell(R) :-
    atom(R),
    !,
    format('<~w>', [R]).
write_cell(X) :-
    format('~p', [X]).


                 /*******************************
                 *         UPDATE RESULTS       *
                 *******************************/

compare_update(Test, Graphs, _Result) :-
    compare_graphs(Graphs, Wrong),
    (   Wrong == []
    ->  assert(passed(Test))
    ;   test_name(Test, Name),
        format('~`=t ~q ~`=t~72|~n', [Name]),
        maplist(write_wrong_graph, Wrong),
        assert(failed(Test))
    ).

compare_graphs([], []).
compare_graphs([graph(G, ApprovedTriples)|T0], Errors) :-
    findall(rdf(S,P,O), rdf(S,P,O,G), OurTriples),
    compare_graphs(ApprovedTriples, OurTriples, Extra, Missing),
    (   Extra == [],
        Missing == []
    ->  Errors = Errors1
    ;   Errors = [graph(G, Extra, Missing)|Errors1]
    ),
    compare_graphs(T0, Errors1).


write_wrong_graph(graph(G, Extra, Missing)) :-
    length(Extra, ExtraCount),
    length(Missing, MissingCount),
    format('GRAPH: ~q; ~D missing, ~D extra triples~n',
           [G, ExtraCount, MissingCount]),
    write_list('MISSED:', Extra, 8),
    write_list('EXTRA:', Missing, 8),
    format('~`=t~72|~n~n', []).

compare_graphs(ApprovedTriples, OurTriples, Extra, Missing) :-
    var_blank_nodes_in_rows(OurTriples, OurV),
    var_blank_nodes_in_rows(ApprovedTriples, ApprovedV),
    sort(OurV, OurRows),
    sort(ApprovedV, ApprovedRows),
    match_rows(OurRows, ApprovedRows, Extra, Missing),
    !.


                 /*******************************
                 *          RDF RESULTS         *
                 *******************************/

%!  result_to_prolog(+Type, +Test, -Result)
%
%   Turn the RDF result into a   more  manageble form. For CONSTRUCT
%   and DESCRIBE queries the result is  a   set  of triples. For the
%   others the format is described below:
%
%           * ASK
%           ask(Bool)
%
%           * SELECT
%           select(names(Name1, ...),
%                  [ row(V1, ...),
%                    ...
%                  ])
%
%           * UPDATE
%           List of graph(Graph, Triples)

result_to_prolog(update, Test, Result) :-
    !,
    findall(graph(Graph, Triples),
            ( test_result_graph(Test, File, Graph),
              load_triples(File, Triples, [base_uri(Graph)])
            ),
            Result).
result_to_prolog(Type, Test, Result) :-
    (   test_result_file(Test, ResultFile)
    ->  (   file_name_extension(_, Ext, ResultFile),
            catch(read_result_file(Ext, Type, ResultFile, Result), E,
                  ( print_message(error, E),
                    fail
                  ))
        ->  true
        ;   test_name(Test, Name),
            format('FAILED to interpret results for ~q~n', [Name]),
            assert(failed_result(Test, ResultFile)),
            fail
        )
    ;   Result = no_result
    ).

read_result_file(srx, _, File, Result) :-
    !,
    sparql_read_xml_result(File, Result).
read_result_file(srj, _, File, Result) :-
    !,
    sparql_read_json_result(File, Result).
read_result_file(tsv, _, File, Result) :-
    !,
    sparql_read_tsv_result(File, Result).
read_result_file(csv, _, File, Result) :-
    !,
    sparql_read_csv_result(File, Result).
read_result_file(_, Type, File, Result) :-
    (   Type == construct
    ;   Type == describe
    ),
    !,
    load_triples(File, Result, []).
read_result_file(_, _, File, Result) :-
    rdf_reset_db,
    rdf_load(File),
    prolog_result(Result),
    rdf_reset_db.


prolog_result(ask(True)) :-
    rdf(Result, rdf:type, r:'ResultSet'),
    rdf(Result, r:boolean, literal(type(xsd:boolean, True))),
    !.
prolog_result(select(ColTerm, Rows)) :-
    rdf(Result, rdf:type, r:'ResultSet'),
    colnames(Result, ColNames),
    ColTerm =.. [names|ColNames],
    result_rows(Result, ColNames, Rows).

colnames(Result, ColNames) :-
    findall(N, rdf(Result, r:resultVariable, literal(N)), ColNames).

result_rows(Result, ColNames, Rows) :-
    findall(R, result_row(Result, ColNames, R), Rows).

result_row(Result, ColNames, Row) :-
    rdf(Result, r:solution, S),
    result_values(ColNames, S, Values),
    Row =.. [row|Values].

result_values([], _, []).
result_values([Name|Names], S, [Value|Values]) :-
    (   rdf(S, r:binding, Binding),
        rdf(Binding, r:variable, literal(Name)),
        rdf(Binding, r:value, Value)
    ->  true
    ;   Value = '$null$'
    ),
    result_values(Names, S, Values).


sparql_read_tsv_result(File, Result) :-
    sparql_read_sv_result(File, turtle, Result,
                          [ separator(0'\t),
                            ignore_quotes(true)
                          ]).
sparql_read_csv_result(File, Result) :-
    sparql_read_sv_result(File, fuzzy, Result,
                          [ separator(0',),
                            ignore_quotes(false)
                          ]).

sparql_read_sv_result(File, Cell, select(NamesTerm, Rows), Options) :-
    csv_read_file(File, Rows0,
                  [ strip(true),
                    convert(false),
                    match_arity(false)
                  | Options
                  ]),
    Rows0 = [NamesRow|DataRows],
    functor(NamesRow, _, Arity),
    NamesRow =.. [_|SparqlNames],
    maplist(csv_colname, SparqlNames, Names),
    NamesTerm =.. [names|Names],
    maplist(cvs_row(Arity, Cell), DataRows, Rows).

csv_colname(Name0, Name) :-
    atom_concat(?, Name, Name0),
    !.
csv_colname(Name, Name).

cvs_row(Arity, Cell, CSVRow, RDFRow) :-
    functor(CSVRow, _, RowArity),
    Missing is Arity-RowArity,
    length(Extra, Missing),
    maplist(=('$null$'), Extra),
    CSVRow =.. [Name|CSVCols],
    maplist(csv_cell(Cell), CSVCols, RDFCols0),
    append(RDFCols0, Extra, RDFCols),
    RDFRow =.. [Name|RDFCols].

csv_cell(turtle, '', '$null$') :- !.
csv_cell(turtle, CSV, RDF) :-
    setup_call_cleanup(
        new_memory_file(MF),
        turtle_object(MF, CSV, RDF),
        free_memory_file(MF)).
csv_cell(fuzzy, CSV, RDF) :-
    (   uri_is_global(CSV)
    ->  RDF = CSV
    ;   atom_concat('_:', NodeID, CSV)
    ->  RDF = bnode(NodeID)
    ;   CSV == ''
    ->  RDF = '$null$'
    ;   atom_number(CSV, Num)
    ->  (   integer(Num)
        ->  to_rdf(integer(CSV), RDF)
        ;   to_rdf(decimal(CSV), RDF)
        )
    ;   RDF = literal(CSV)
    ).


turtle_object(MF, Atom, Object) :-
    setup_call_cleanup(
        open_memory_file(MF, write, Out),
        format(Out, '<http://s> <http://p> ~w .', [Atom]),
        close(Out)),
    setup_call_cleanup(
        open_memory_file(MF, read, In),
        rdf_read_turtle(stream(In), [rdf(_,_,Object)], []),
        close(In)).


:- rdf_meta to_rdf(+, t).

to_rdf(integer(Atom), literal(type(xsd:integer, Atom))).
to_rdf(double(Atom),  literal(type(xsd:double, Atom))).
to_rdf(decimal(Atom), literal(type(xsd:decimal, Atom))).
to_rdf(string(Atom),  literal(type(xsd:string, Atom))).


                 /*******************************
                 *          SYNTAX TESTS        *
                 *******************************/

%!  run_syntax_tests
%
%   Load both the SyntaxDev  and  all   normal  tests  and runs them
%   through the parser. Does not involve any semantic checking.

run_syntax_tests :-
    clean_tests,
    load_syntax_manifests,
    run_all_syntax_tests.

load_syntax_manifests :-
    load_manifests([ sparql11,
                     'Tests/sparql/test-suite-archive/data-r2/manifest-syntax.ttl'
                   ]).


                 /*******************************
                 *         RUNNING TESTS        *
                 *******************************/

run_all_syntax_tests :-
    clean_test_results,
    forall(current_test(_, Test),
           (   blocked_test(Test)
           ->  assertz(skipped(Test, blocked))
           ;   syntax_test(Test)
           ->  true
           ;   format('FAILED: ~q~n', [Test]),
               fail
           )),
    test_statistics.

test_statistics :-
    findall(T, passed(T), Passed), length(Passed, NPassed),
    findall(T, failed(T), Failed), length(Failed, NFailed),
    findall(W-T, skipped(T, W), Skipped), length(Skipped, NSkipped),
    keysort(Skipped, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    maplist(group_size, Grouped, Sized),
    format('Passed: ~D; failed: ~D; skipped: ~D ~w~n',
           [NPassed, NFailed, NSkipped, Sized]).

group_size(Group-Tests, Group-Size) :-
    length(Tests, Size).


syntax_test(Name) :-
    syntax_test(Name, _Query).

syntax_test(Name, Query) :-
    test_name(Test, Name),
    !,
    syntax_test(Test, Query).
syntax_test(Test, Query) :-
    test_query(Test, Codes),
    !,
    syntax_test(Test, Codes, Query).
syntax_test(Test, _) :-
    assertz(skipped(Test, no_query)).

blocked_test(Test) :-
    test_name(Test, Name),
    blocked(Name).

syntax_test(Test, Codes, Query) :-
    test_syntax(Test, negative),
    !,
    (   catch(sparql_parse(Codes, Query, []), E, true)
    ->  (   nonvar(E)
        ->  assert(passed(Test))
        ;   test_name(Test, Name),
            format('NEG TEST SUCCEEDED: ~q:~n', [Name]),
            assert(failed(Test))
        )
    ;   test_failed(Test, 'NEG SYNTAX TEST SUCCEEDED')
    ).
syntax_test(Test, Codes, Query) :-
    test_syntax(Test, _),
    !,
    (   catch(sparql_parse(Codes, Query, []), E, true)
    ->  (   var(E)
        ->  assert(passed(Test))
        ;   test_name(Test, Name),
            format('PARSE TEST ERROR: ~q: ', [Name]),
            print_message(error, E),
            assert(failed(Test))
        )
    ;   test_failed(Test, 'POS SYNTAX TEST FAILED')
    ).
syntax_test(Test, _, _) :-
    assert(skipped(Test, no_syntax_test)).


test_failed(Test, Msg) :-
    assert(failed(Test)),
    test_id(Test, Id),
    format('~w: ~q~n', [Msg, Id]).

test_id(Test, Id) :-
    test_name(Test, Name),
    (   \+ ( test_name(Test2, Name),
             Test2 \== Test
           )
    ->  Id = Name
    ;   Id = Test
    ).


                 /*******************************
                 *         QUERY LISTING        *
                 *******************************/

test_query_listing :-
    (   current_test(_, Test),
        test_syntax(Test, positive),
        test_query_listing(Test),
        fail ; true
    ).

test_query_listing(Name) :-
    test_name(Test, Name),
    !,
    test_query_listing(Test).
test_query_listing(Test) :-
    test_name(Test, Name),
    test_query(Test, Codes),
    format('~`=t ~w ~`=t~72|~n', [Name]),
    format('~s', [Codes]),
    format('~`-t~72|~n'),
    (   catch(sparql_parse(Codes, Query, []), E, true)
    ->  (   var(E)
        ->  list_query(Query)
        ;   print_message(error, E)
        )
    ;   format('FAILED TO PARSE~n')
    ),
    format('~`=t~72|~n~n').

list_query(select(Vars, _, Query, _)) :-
    portray_clause(select(Vars) :- Query).
list_query(construct(Templ, _, Query, _)) :-
    portray_clause(construct(Templ) :- Query).
list_query(ask(_, Query)) :-
    portray_clause(ask :- Query).
list_query(describe(Vars, _, Query, _)) :-
    portray_clause(describe(Vars) :- Query).


                 /*******************************
                 *      GENERIC TEST STUFF      *
                 *******************************/

clean_tests :-
    clean_test_results,
    reset_manifests,
    rdf_reset_db.

clean_test_results :-
    retractall(failed_result(_, _)),
    retractall(passed(_)),
    retractall(failed(_)),
    retractall(skipped(_, _)).

list_tests(Which) :-
    findall(Test, test_result(Which, Test), Tests),
    sort(Tests, Sorted),
    upcase_atom(Which, Message),
    forall(member(Test, Sorted),
           format('~w: ~q~n', [Message, Test])).

:- meta_predicate test_result(+, -).

test_result(skipped(Why), Name) :-
    !,
    skipped(Test, Why),
    test_name(Test, Name).
test_result(skipped, Name) :-
    !,
    skipped(Test, _),
    test_name(Test, Name).
test_result(Which, Name) :-
    call(Which, Test),
    test_name(Test, Name).

list_db :-
    findall(Graph, rdf_graph(Graph), Graphs),
    list_dbs(Graphs).

list_dbs([Graph|More]) :-
    ansi_format([bold], '### Graph ~q ###~n', [Graph]),
    rdf_save_turtle(stream(current_output),
                    [ graph(Graph),
                      silent(true),
                      comment(false)
                    ]),
    (   More == []
    ->  true
    ;   format('~n'),
        list_dbs(More)
    ).

%!  dump_tests(+File) is det.
%
%   Dump status of all tests into File

dump_tests(File) :-
    collect_tests(Facts),
    setup_call_cleanup(
        open(File, write, Out),
        forall(member(Fact, Facts),
               format(Out, '~q.~n', [Fact])),
        close(Out)).

collect_tests(Facts) :-
    setof(Fact, test_fact(Fact), Facts).

test_fact(test(Test, no_result(From))) :-
    failed_result(Test, From).
test_fact(test(Test, passed)) :-
    passed(Test).
test_fact(test(Test, failed)) :-
    failed(Test).
test_fact(test(Test, skipped(Why))) :-
    skipped(Test, Why).

compare_tests(To) :-
    read_file_to_terms(To, Old, []),
    collect_tests(New),
    compare(Old, New).

compare([], []).
compare([H|T0], [H|T1]) :-
    !,
    compare(T0, T1).
compare([test(T,R0)|T0], [test(T,R1)|T1]) :-
    !,
    test_name(T, Name),
    format('~q: ~q --> ~q~n', [Name, R0, R1]),
    compare(T0, T1).
compare([H0|T0], [H1|T1]) :-
    (   H0 @< H1
    ->  H0 = test(Test, Result),
        test_name(Test, Name),
        format('Old: ~q: ~q~n', [Name, Result]),
        compare(T0, [H1|T1])
    ;   H1 = test(Test, Result),
        test_name(Test, Name),
        format('New: ~q: ~q~n', [Name, Result]),
        compare([H0|T0], T1)
    ).


                 /*******************************
                 *              DEBUG           *
                 *******************************/

:- portray_text(true).

user:portray(IRI) :-
    atom(IRI),
    rdf_global_id(NS:Local, IRI),
    Local \== '',
    format('~w:~w', [NS, Local]).


:- multifile
    prolog:message//1.

prolog:message(sparql(no_test_data)) -->
    [ 'No test data.'-[] ].
