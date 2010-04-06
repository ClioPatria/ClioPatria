/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(test_sparql,
	  [ sparql_parse/3,		% +Text, -Query, +Options

	    load_manifests/1,		% Load 'arq' or 'dawg' manifests

	    show_test/1,		% +NameOrIRI
	    show_test_data/1,		% +NameOrIRI
	    edit_test_data/1,		% +NameOrIRI
	    edit_test_result/1,		% +NameOrIRI
	    edit_test/1,		% +NameOrIRI
	    list_tests/1,		% +Class
	    list_db/0,

					% SYNTAX TESTS
	    syntax_test/1,		% +NameOrIRI
	    syntax_test/2,		% +NameOrIRI, -Query
	    run_syntax_tests/0,
	    test_query_listing/0,
	    test_query_listing/1,	% +NameOrIRI

					% QUERY TESTS
	    run_query_tests/0,
	    query_test/1		% +NameOrIRI
	  ]).
:- use_module(sparql_grammar).
:- use_module(sparql).
:- use_module(library('semweb/rdf_db')).
:- use_module(library(url)).
:- use_module(library(apply)).
:- use_module(test_manifest).
:- use_module(sparql_xml_result).
:- use_module(rdf_entailment, []).
:- use_module(no_entailment, []).
					% Toplevel debugging utilities
:- use_module(user:library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle_write)).

:- dynamic
	failed_result/2,
	passed/1,
	failed/1,
	skipped/1.

%%	blocked(?Name)
%
%	Blocked tests

:- multifile
	blocked/1.

blocked('extendedType-literal-ne').
blocked('typePromotion-decimal-decimal-pass').
blocked('extendedType-ne-fail').
					% ARQ tests with .srj result file
blocked('strlen - 1').			% is this JSON?
blocked('strlen - 2').
					% ARQ tests with UNSAID
blocked('One optional clause (alt)').
blocked('Two optional clauses (alt)').
blocked('UNSAID - triple/present').
blocked('UNSAID - triple/absent').
blocked('UNSAID of pattern in basic block => false').
blocked('UNSAID of pattern not matching').
blocked('UNSAID of pattern partially matching').


		 /*******************************
		 *	     QUERY TESTS	*
		 *******************************/

%	TBD: As we have to have an  empty database we must first collect
%	all info in the Prolog database :-(  Alternatively we need a way
%	to set the current database (thread-local!)

%	run_query_tests
%
%	Load all manifests and execute the tests.

run_query_tests :-
	clean_tests,
	load_query_manifests,
	run_all_query_tests.

load_query_manifests :-
	load_manifests([ arq,
			 dawg
		       ]).

run_all_query_tests :-
	(   current_test(_, Test),
	    test_name(Test, Name),
	    (	blocked(Name)
	    ->	assert(skipped(Test)),
		fail
	    ;	true
	    ),
	    query_test(Test),
	    fail ; true
	),

	findall(T, passed(T), Passed), length(Passed, NPassed),
	findall(T, failed(T), Failed), length(Failed, NFailed),
	findall(T, skipped(T), Skipped), length(Skipped, NSkipped),
	format('Passed: ~D; failed: ~D; skipped: ~D~n',
	       [NPassed, NFailed, NSkipped]).

query_test(Name) :-
	test_name(Test, Name), !,
	query_test(Test).
query_test(Test) :-
	test_name(Test, Name),
	format('~`=t BEGIN ~q ~`=t~72|~n', [Name]),
	test_query(Test, Query),
					% Compile the query
	(   catch(sparql_compile(Query, Compiled,
				 [ type(Type),
				   entailment(none)
				 ]), E, true)
	->  (   var(E)
	    ->	true
	    ;	print_message(error, E),
		assert(failed(Test)),
		fail
	    )
	;   format('FAILED to compile ~q~n', [Name]),
	    assert(failed(Test)),
	    fail
	),
					% get the correct result
	result_to_prolog(Type, Test, PrologResult),
					% load the data
	rdf_reset_db,
	test_data_files(Test, DataFiles),
	maplist(rdf_load, DataFiles),

					% run the query
	catch(findall(Result, sparql_run(Compiled, Result), Results),
	      E, true),
	(   var(E)
	->				% compare the result
	    compare_results(Test, Type, PrologResult, Results)
	;   print_message(error, E),
	    assert(failed(Test)),
	    fail
	).


%%	compare_results(+Test, +Type, +Correct, +Results)
%
%	NOTE: Some tests (notably syntax tests)  do not have results. In
%	that case Results is bound to no_result and we do not compare.

compare_results(_, _, no_result, _) :- !.
compare_results(Test, ask, ask(Bool), [Bool]) :- !,
	assert(passed(Test)).
compare_results(Test, Type, select(ColNames, Rows), Result) :-
	Type = select(MyColNames), !,
	same_colnames(ColNames, MyColNames, Map),
	(   Map == nomap
	->  RowsMyOrder = Rows
	;   map_rows(Rows, Map, RowsMyOrder)
	),
	compare_sets(Test, Type, RowsMyOrder, Result).
compare_results(Test, construct, Correct, Result) :- !,
	compare_sets(Test, compare_sets, Correct, Result).
compare_results(Test, ask, ask(Correct), [Result]) :- !,
	test_name(Test, Name),
	format('~`=t ~q ~`=t~72|~n', [Name]),
	format('TYPE: ASK~n'),
	format('CORRECT: ~q~n', [Correct]),
	format('WE: ~q~n', [Result]).
compare_results(Test, Type, Correct, Result) :-
	test_name(Test, Name),
	format('~`=t ~q ~`=t~72|~n', [Name]),
	format('TYPE: ~q~n', [Type]),
	write_list('CORRECT:', Correct, 8),
	write_list('WE:', Result, 8),
	format('~`=t~72|~n~n', []),
	assert(failed(Test)).

compare_sets(Test, Type, RowsMyOrder, Result) :-
	var_blank_nodes_in_rows(RowsMyOrder, RowsMyOrderV),
	var_blank_nodes_in_rows(Result, ResultV),
	sort(RowsMyOrderV, OkRows),		% TBD: match blank nodes!
	sort(ResultV, MyRows),
	match_rows(MyRows, OkRows, MyExtra, OkExtra), !,
	(   MyExtra == [],
	    OkExtra == []
	->  assert(passed(Test))
	;   test_name(Test, Name),
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


%%	same_colnames(+Names1, +Names2, -Map)
%
%	Is true if Names1 and Names2 contain  the same names in possibly
%	different order. If the order is diffent,   Map  is unified to a
%	term map(RowLeft, RowRight), where variable bindings between the
%	row-terms express the mapping between the rows.

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
	nth1(I2, Names, H), !,
	arg(I2, R2, V),
	IN is I + 1,
	fill_vars(T, IN, Names, R1, R2).

map_rows([], _, []).
map_rows([H0|T0], Map, [H|T]) :-
	copy_term(Map, map(H0, H)),
	map_rows(T0, Map, T).


%%	var_blank_nodes_in_rows(+Rows0, -Rows)
%
%	Substitute blank nodes in rows by variables.  Note that blank
%	nodes of multiple rows are not related.

var_blank_nodes_in_rows([], []).
var_blank_nodes_in_rows([H0|T0], [H|T]) :-
	var_blank_nodes(H0, H),
	var_blank_nodes(T0, T).


%%	var_blank_nodes(+Term, -VarBlanks)
%
%	Process Term, replacing blank nodes with variables.

var_blank_nodes(Term0, Term) :-
	empty_assoc(Vars),
	var_blank_nodes(Term0, Vars, _, Term).

var_blank_nodes(BN, Vars0, Vars, V) :-
	rdf_is_bnode(BN), !,
	(   get_assoc(BN, Vars0, V)
	->  Vars = Vars0
	;   put_assoc(BN, Vars0, V, Vars)
	).
var_blank_nodes(Term0, Vars0, Vars, Term) :-
	compound(Term0), !,
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


%%	match_rows(+Rows0, +Rows1, -Extra1, -Extra2)
%
%	Succeed if both sets of rows are   the same. Note that there may
%	be blank nodes in the  rows.   These  are already substituted by
%	Prolog variables. This is basically a permutation problem. First
%	we decide to which row each row can  match. Then we sort them to
%	the  lowest  number  of  matches  and    finally  we  start  the
%	non-deterministic matching process.

match_rows(Rows1, Rows2, Extra1, Extra2) :-
	candidate_matches(Rows1, Rows2, Extra1, Extra1Tail, Candidates),
	keysort(Candidates, Sorted),
	same_rows(Sorted, Rows2, Extra1Tail, Extra2).

candidate_matches([], _, E, E, []).
candidate_matches([H0|T0], Rows, E0, E, [L-Rs|T]) :-
	setof(R, member_row(H0, R, Rows), Rs), !,
	length(Rs, L),
	candidate_matches(T0, Rows, E0, E, T).
candidate_matches([H0|T0], Rows, [H0|E0], E, T) :-
	candidate_matches(T0, Rows, E0, E, T).

same_rows(E, [], E, []) :- !.
same_rows([], E, [], E) :- !.
same_rows([_-Rs|T], Rows, E1, E2) :-
	member(R, Rs),
	select_eq(R, Rows, Rest),
	same_rows(T, Rest, E1, E2), !.

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
same_value(literal(type(T,V1)), literal(type(T,V2))) :-
	xsdp_numeric_uri(T, _),
	to_number(V1, N1),
	to_number(V2, N2),
	N1 =:= N2.

to_number(a, _) :- !, fail.		% catch variables
to_number(N, N) :-
	number(N), !.
to_number(A, N) :-
	catch(atom_number(A, N), _, fail).


%%	write_list(+Prompt, +List, +Indent)
%
%	Write list of rows or rdf terms.

write_list(_, [], _) :- !.
write_list(Prompt, List, Indent) :- !,
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

write_cell(literal(X)) :- !,
	format('"~w"', [X]).
write_cell(R) :-
	atom(R),
	rdf_global_id(NS:Id, R), !,
	format('<~w:~w>', [NS, Id]).
write_cell('$null$') :- !,
	write('NULL').
write_cell(R) :-
	atom(R), !,
	format('<~w>', [R]).
write_cell(X) :-
	format('~p', [X]).


		 /*******************************
		 *	    RDF RESULTS		*
		 *******************************/

%%	result_to_prolog(+Type, +Test, -Result)
%
%	Turn the RDF result into a   more  manageble form. For CONSTRUCT
%	and DESCRIBE queries the result is  a   set  of triples. For the
%	others the format is described below:
%
%		# ASK
%%		ask(Bool)
%
%		# SELECT
%		select([Name1, ...],
%		       [ row(V1, ...),
%			 ...
%		       ])

result_to_prolog(Type, Test, Result) :-
	(   test_result_file(Test, ResultFile)
	->  (   file_name_extension(_, Ext, ResultFile),
	        (   Ext == srx
		->  sparql_read_xml_result(ResultFile, Result)
		;   (   Type == construct
		    ;	Type == describe
		    )
		->  load_triples(ResultFile, Result)
		;   rdf_reset_db,
		    rdf_load(ResultFile),
		    prolog_result(Result),
		    rdf_reset_db
		)
	    ->	true
	    ;	test_name(Test, Name),
		format('FAILED to interpret results for ~q~n', [Name]),
		assert(failed_result(Test, ResultFile)),
		fail
	    )
	;   Result = no_result
	).

prolog_result(ask(True)) :-
	rdf(Result, rdf:type, r:'ResultSet'),
	rdf(Result, r:boolean, literal(type(xsd:boolean, True))), !.
prolog_result(select(ColNames, Rows)) :-
	rdf(Result, rdf:type, r:'ResultSet'),
	colnames(Result, ColNames),
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


		 /*******************************
		 *	    SYNTAX TESTS	*
		 *******************************/

%%	run_syntax_tests
%
%	Load both the SyntaxDev  and  all   normal  tests  and runs them
%	through the parser. Does not involve any semantic checking.

run_syntax_tests :-
	clean_tests,
	load_syntax_manifests,
	run_all_syntax_tests.

load_syntax_manifests :-
	load_manifests([ arq,
			 'Tests/sparql/test-suite-archive/data-r2/manifest-syntax.ttl'
		       ]).


		 /*******************************
		 *	   RUNNING TESTS	*
		 *******************************/

run_all_syntax_tests :-
	forall(current_test(_, Test),
	       (   blocked_test(Test)
	       ->  assertz(skipped(Test))
	       ;   syntax_test(Test)
	       ->  true
	       ;   format('FAILED: ~q~n', [Test]),
		   fail
	       )),
	findall(T, passed(T), Passed), length(Passed, NPassed),
	findall(T, failed(T), Failed), length(Failed, NFailed),
	findall(T, skipped(T), Skipped), length(Skipped, NSkipped),
	format('Passed: ~D; failed: ~D; skipped: ~D~n',
	       [NPassed, NFailed, NSkipped]).

syntax_test(Name) :-
	syntax_test(Name, _Query).

syntax_test(Name, Query) :-
	test_name(Test, Name), !,
	syntax_test(Test, Query).
syntax_test(Test, Query) :-
	test_query(Test, Codes), !,
	syntax_test(Test, Codes, Query).
syntax_test(Test, _) :-
	assertz(skipped(Test)).

blocked_test(Test) :-
	test_name(Test, Name),
	blocked(Name).

syntax_test(Test, Codes, Query) :-
	test_syntax(Test, positive), !,
	(   catch(sparql_parse(Codes, Query, []), E, true)
	->  (   var(E)
	    ->  assert(passed(Test))
	    ;   test_name(Test, Name),
		format('PARSE TEST ERROR: ~q: ', [Name]),
		print_message(error, E),
		assert(failed(Test))
	    )
	;   assert(failed(Test)),
	    test_name(Test, Name),
	    format('PARSE TEST FAILED: ~q~n', [Name])
	).
syntax_test(Test, Codes, Query) :-
	test_syntax(Test, negative), !,
	(   catch(sparql_parse(Codes, Query, []), E, true)
	->  (   nonvar(E)
	    ->  assert(passed(Test))
	    ;   test_name(Test, Name),
		format('NEG TEST SUCCEEDED: ~q:~n', [Name]),
		assert(failed(Test))
	    )
	;   assert(failed(Test)),
	    test_name(Test, Name),
	    format('NEG TEST FAILED WITHOUT ERROR: ~q:~n', [Name])
	).
syntax_test(Test, _, _) :-
	assert(skipped(Test)).


		 /*******************************
		 *	   QUERY LISTING	*
		 *******************************/

test_query_listing :-
	(   current_test(_, Test),
	    test_syntax(Test, positive),
	    test_query_listing(Test),
	    fail ; true
	).

test_query_listing(Name) :-
	test_name(Test, Name), !,
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
	    ;	print_message(error, E)
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
		 *	GENERIC TEST STUFF	*
		 *******************************/

clean_tests :-
	retractall(failed_result(_, _)),
	retractall(passed(_)),
	retractall(failed(_)),
	retractall(skipped(_)),
	reset_manifests,
	rdf_reset_db.

list_tests(passed) :-
	forall(passed(Test),
	       (   test_name(Test, Name),
		   format('PASSED: ~q~n', [Name]))).
list_tests(failed) :-
	forall(failed(Test),
	       (   test_name(Test, Name),
	           format('FAILED: ~q~n', [Name]))).
list_tests(skipped) :-
	forall(skipped(Test),
	       (   test_name(Test, Name),
		   format('SKIPPED: ~q~n', [Name]))).

list_db :-
	rdf_save_turtle(stream(current_output), []).


		 /*******************************
		 *		DEBUG		*
		 *******************************/

:- portray_text(true).

user:portray(IRI) :-
	atom(IRI),
	rdf_global_id(NS:Local, IRI),
	Local \== '',
	format('~w:~w', [NS, Local]).
