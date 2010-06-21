/*  This file is part of ClioPatria.

    Author:
    HTTP:	http://e-culture.multimedian.nl/
    GITWEB:	http://gollem.science.uva.nl/git/ClioPatria.git
    GIT:	git://gollem.science.uva.nl/home/git/ClioPatria.git
    GIT:	http://gollem.science.uva.nl/home/git/ClioPatria.git
    Copyright:  2007, E-Culture/MultimediaN

    ClioPatria is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    ClioPatria is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ClioPatria.  If not, see <http://www.gnu.org/licenses/>.
*/

:- module(sesame_tests,
	  [ test/1,
	    test/0,
	    view/1,
	    in/1
	  ]).
:- use_module(serql).
:- use_module(library(rdf_ntriples)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library(readutil)).
:- use_module(library(debug)).
:- use_module(rdfs_entailment, []).

:- multifile
	user:file_search_path/2.

user:file_search_path(sesame, '/staff/jan/src/openrdf').


test_dir(construct, Dir) :-
	absolute_file_name(sesame('test/files/testcases/SeRQL/CfwQuery'), Dir,
			   [ file_type(directory),
			     access(read)
			   ]).

assert_rdfs_triples :-
	catch(nb_getval(rdfs_triples, Triples), _, fail), !,
	forall(member(rdf(S,P,O), Triples), rdf_assert(S,P,O)).
assert_rdfs_triples :-
	absolute_file_name(sesame('test/files/testcases/SeRQL/CfwQuery/test000-out.nt'), NT,
			   [ access(read)
			   ]),
	load_rdf_ntriples(NT, Triples),
	nb_setval(rdfs_triples, Triples),
	forall(member(rdf(S,P,O), Triples), rdf_assert(S,P,O)).

test :-
	test_dir(construct, Dir),
	atom_concat(Dir, '/*-query', Pattern),
	expand_file_name(Pattern, Files),
	maplist(test_base, Files, Bases),
	(   member(Base, Bases),
	    (	test_file(Base, true)
	    ->	put('.'), flush_output
	    ;	format('Test failed: ~w~n', [Base])
	    ),
	    fail
	;   nl
	).

test_base(File, Base) :-
	atom_concat(Base, '-query', File).


test(N) :-
	test_dir(construct, Dir),
	sformat(File, '~`0t~d~3|', [N]),
	concat_atom([Dir, '/test', File], Base),
	test_file(Base).

view(N) :-
	test_dir(construct, Dir),
	sformat(File, '~`0t~d~3|', [N]),
	concat_atom([Dir, '/test', File], Base),
	atom_concat(Base, '-query', Query),
	edit(Query).
in(N) :-
	test_dir(construct, Dir),
	sformat(File, '~`0t~d~3|', [N]),
	concat_atom([Dir, '/test', File], Base),
	atom_concat(Base, '-in.nt', Query),
	edit(Query).

test_file(Base) :-
	test_file(Base, false).

test_file(Base, Silent) :-
	atom_concat(Base, '-query', Query),
	atom_concat(Base, '-in.nt', InFile),
	atom_concat(Base, '-out.nt', OutFile),
	read_file_to_codes(Query, Codes, []),
	atom_codes(Text, Codes),	% easier debugging
	rdf_reset_db,
	assert_rdfs_triples,
	load_rdf_ntriples(InFile, Triples),
	forall(member(rdf(S,P,O), Triples), rdf_assert(S,P,O)),
	findall(Statement,
		serql_query(Text, Statement, [entailment(rdfs)]),
		Statements),
	load_rdf_ntriples(OutFile, OutTriples),
	(   compare_triples(Statements, OutTriples, _Subst)
	->  true
	;   (   Silent == true
	    ->  fail
	    ;   report_difference(Statements, OutTriples),
		edit(Query)
	    )
	).


report_difference(Statements, OutTriples) :-
	format(user_error, 'WRONG ANSWER~n~n', []),
	format(user_error, '    *** Result ***:~n', []),
	list_triples(Statements, user_error),
	format(user_error, '    *** Official Result ***:~n', []),
	list_triples(OutTriples, user_error).


list_triples([], _).
list_triples([rdf(S,P,O)|T], Stream) :-
	rdf_global_id(LS, S),
	rdf_global_id(LP, P),
	(   nonvar(O), O = literal(_)
	->  LO = O
	;   rdf_global_id(LO, O)
	),
	format(Stream, 'rdf(~p, ~p, ~p).~n', [LS, LP, LO]),
	list_triples(T, Stream).


		 /*******************************
		 *	     COMPARING		*
		 *******************************/

%%	compare_triples(+PlRDF, +NTRDF, -Substitions)
%
%	Compare two models and if they are equal, return a list of
%	PlID = NTID, mapping NodeID elements.


compare_triples(A, B, Substitutions) :-
	compare_list(A, B, [], Substitutions).

compare_list([], [], S, S).
compare_list(L1, L2, S0, S) :-
	take_bag(L1, B1, E1, R1), !,
	take_bag(L2, B2, E2, R2),
	compare_field(B1, B2, S0, S1),
	compare_bags(E1, E2, S1, S2),
	compare_list(R1, R2, S2, S).
compare_list([H1|T1], In2, S0, S) :-
	select(H2, In2, T2),
	compare_triple(H1, H2, S0, S1), % put(.), flush_output,
	compare_list(T1, T2, S1, S).

compare_triple(rdf(Subj1,P1,O1), rdf(Subj2, P2, O2), S0, S) :-
	compare_field(Subj1, Subj2, S0, S1),
	compare_field(P1, P2, S1, S2),
	compare_field(O1, O2, S2, S).

compare_field(X, X, S, S) :- !.
compare_field(literal(X), xml(X), S, S) :- !. % TBD
compare_field(NS:Name, Atom, S, S) :-
	rdf_global_id(NS:Name, Atom), !.
compare_field(X, node(Id), S, S) :-
	memberchk(X=Id, S), !.
compare_field(X, node(Id), S, [X=Id|S]) :-
	\+ memberchk(X=_, S),
	atom(X),
	rdf_is_bnode(X), !,
	debug(bnode, 'Assume ~w = ~w~n', [X, node(Id)]).


%%	compare_bags(+Members1, +Members2, +S0, -S)
%	
%	Order of _1, _2, etc. are not relevant in BadID reification. Are
%	they in general?  Anyway, we'll normalise the order of the bags

compare_bags([], [], S, S).
compare_bags([E1|T1], M, S0, S) :-
	select(E2, M, T2),
	compare_field(E1, E2, S0, S1),
	compare_bags(T1, T2, S1, S).

take_bag(Triples, Bag, Elems, RestTriples) :-
	select(rdf(Bag, Type, BagClass), Triples, T1),
	compare_field(rdf:type, Type, [], []),
	compare_field(rdf:'Bag', BagClass, [], []),
	bag_members(T1, Bag, Elems, RestTriples).
	
bag_members([], _, [], []).
bag_members([rdf(Bag, IsElm, E)|T], Bag, [E|ET], Rest) :-
	member_prop(IsElm), !,
	bag_members(T, Bag, ET, Rest).
bag_members([T0|T], Bag, Elems, [T0|R]) :-
	bag_members(T, Bag, Elems, R).
	
member_prop(rdf:Name) :-
	atom_codes(Name, [0'_|Codes]),
	number_codes(_N, Codes), !.
member_prop(Prop) :-
	atom(Prop),
	rdf_parser:rdf_name_space(NS),
	atom_concat(NS, Name, Prop),
	atom_codes(Name, [0'_|Codes]),
	number_codes(_N, Codes), !.
