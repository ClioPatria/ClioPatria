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

:- use_module(serql).

query(s1, 'select * from {S} P {O}').
query(s2, 'select * from {S} P {O1, O2}').
query(s3, 'select * from {S} P {O1};
 			     Q {O2}').
query(s4, 'select * from {S} P1 {O1} P2 {O2}').
					% fig 5.5
query(s5, 'select * from {first} pred {} pred1 {obj1};
				 	 pred2 {obj2} pred3 {obj3}').
query(s6, 'select * from {S} P {O} limit 10').
query(s7, 'select * from {S} P {O} limit 10 offset 10').
query(s8, 'select * from {S} <rdf:type> {<rdfs:Class>}').
query(s9, 'select * from {S} <rdfs:label> {"jan"}').
query(s10, 'select * from {S} <rdfs:label> {"jan"@nl}').
query(s11, 'select * from {S} <rdfs:label> {L} where L like "jan*"').

query(w1, 'select * from {S} P {O} where O > "10"').
query(w2, 'select * from {S} P {O} where O > "10"^^<xsd:positiveInt>').

query(c1, 'construct * from {S} <rdf:type> {P}').
query(c2, 'construct {P} <rdf:hasInstance> {S} from {S} <rdf:type> {P}').

t(Q) :-
	query(Q, Codes),
	serql_compile(Codes, Query, []),
	print(Query).

:- multifile
	ok/2.

test(Q) :-
	query(Q, Text),
	serql_compile(Text, Query, []),
%%	numbervars(Query, 0, _, [singleton(true)]),
%%	format('ok(~q, ~W).~n', [Q, Query, [numbervars(true), quoted(true)]]),
	(   ok(Q, Reply)
	->  (   Reply =@= Query
	    ->  true
	    ;   format(user_error, 'ERROR: failed on ~w~n', [Q])
	    )
	;   name_vars(Query),
	    numbervars(Query, 0, _),
	    format('ok(~q, ~p).~n', [Q, Query])
	).

name_vars(select(Row, Names, _Path, _Where, _Limit, _Offset)) :- !,
	functor(Row, _, Arity),
	name_vars(0, Arity, Row, Names).
name_vars(_).

name_vars(Arity, Arity, _, _) :- !.
name_vars(I0, Arity, Row, Names) :-
	I is I0 + 1,
	arg(I, Names, Name),
	pl_var_name(Name, VarName),
	arg(I, Row, '$VAR'(VarName)),
	name_vars(I, Arity, Row, Names).

pl_var_name(Name, Name) :-
	sub_atom(Name, 0, 1, _, First),
	char_type(First, upper), !.
pl_var_name(Name, Upper) :-
	atom_concat('_', Name, Upper).

user:portray('$VAR'(Name)) :-
	atom(Name),
	write(Name).

test :-
	(   query(Q, _),
	    test(Q),
	    fail
	;   true
	).

ok(s1, serql_query(select((rdf(A, B, C), true, true),
			  solutions(unsorted, inf, 0)),
		   row(A, B, C),
		   rdfs_entailment)).
ok(s1, serql_query(select((rdf(A, B, C), true, true), solutions(unsorted, inf, 0)), row(A, B, C), rdfs_entailment)).
ok(s2, serql_query(select(((rdf(A, B, C), true, rdf(A, B, D), C\==D), true, true), solutions(unsorted, inf, 0)), row(A, B, C, D), rdfs_entailment)).
ok(s2, serql_query(select(((rdf(A, B, C), true, rdf(A, B, D), C\==D), true, true), solutions(unsorted, inf, 0)), row(A, B, C, D), rdfs_entailment)).
ok(s3, serql_query(select(((rdf(A, B, C), rdf(A, D, E)), true, true), solutions(unsorted, inf, 0)), row(A, B, C, D, E), rdfs_entailment)).
ok(s3, serql_query(select(((rdf(A, B, C), rdf(A, D, E)), true, true), solutions(unsorted, inf, 0)), row(A, B, C, D, E), rdfs_entailment)).
ok(s4, serql_query(select(((rdf(A, B, C), rdf(C, D, E)), true, true), solutions(unsorted, inf, 0)), row(A, B, C, D, E), rdfs_entailment)).
ok(s4, serql_query(select(((rdf(A, B, C), rdf(C, D, E)), true, true), solutions(unsorted, inf, 0)), row(A, B, C, D, E), rdfs_entailment)).
ok(s5, serql_query(select((rdf(A, B, C), rdfql_carthesian([bag([D, E], rdf(C, D, E)), bag([F, G, H, I], (rdf(C, F, G), rdf(G, H, I)))])), solutions(unsorted, inf, 0)), row(A, B, D, E, F, G, H, I), rdfs_entailment)).
ok(s5, serql_query(select((rdf(A, B, C), rdfql_carthesian([bag([D, E], rdf(C, D, E)), bag([F, G, H, I], (rdf(C, F, G), rdf(G, H, I)))])), solutions(unsorted, inf, 0)), row(A, B, D, E, F, G, H, I), rdfs_entailment)).
ok(s6, serql_query(select((rdf(A, B, C), true, true), solutions(unsorted, 10, 0)), row(A, B, C), rdfs_entailment)).
ok(s6, serql_query(select((rdf(A, B, C), true, true), solutions(unsorted, 10, 0)), row(A, B, C), rdfs_entailment)).
ok(s7, serql_query(select((rdf(A, B, C), true, true), solutions(unsorted, 10, 10)), row(A, B, C), rdfs_entailment)).
ok(s7, serql_query(select((rdf(A, B, C), true, true), solutions(unsorted, 10, 10)), row(A, B, C), rdfs_entailment)).
ok(s8, serql_query(select((rdf(A, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2000/01/rdf-schema#Class'), true, true), solutions(unsorted, inf, 0)), row(A), rdfs_entailment)).
ok(s9, serql_query(select((rdf(A, 'http://www.w3.org/2000/01/rdf-schema#label', literal(jan)), true, true), solutions(unsorted, inf, 0)), row(A), rdfs_entailment)).
ok(s10, serql_query(select((rdf(A, 'http://www.w3.org/2000/01/rdf-schema#label', literal(lang(nl, jan))), true, true), solutions(unsorted, inf, 0)), row(A), rdfs_entailment)).
ok(s11, serql_query(select((rdf(A, 'http://www.w3.org/2000/01/rdf-schema#label', B), serql_compare(like, B, 'jan*'), true), solutions(unsorted, inf, 0)), row(A, B), rdfs_entailment)).
ok(w1, serql_query(select((rdf(A, B, C), serql_compare(>, C, query('10')), true), solutions(unsorted, inf, 0)), row(A, B, C), rdfs_entailment)).
ok(w1, serql_query(select((rdf(A, B, C), serql_compare(>, C, query(10)), true), solutions(unsorted, inf, 0)), row(A, B, C), rdfs_entailment)).
ok(w2, serql_query(select((rdf(A, B, C), serql_compare(>, C, query(type('http://www.w3.org/2001/XMLSchema#positiveInt', '10'))), true), solutions(unsorted, inf, 0)), row(A, B, C), rdfs_entailment)).
ok(c1, serql_query(construct(((rdf(A, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', B), true), serql_member_statement(C, [rdf(A, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', B)])), solutions(unsorted, inf, 0)), C, rdfs_entailment)).
ok(c2, serql_query(construct(((rdf(A, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', B), true), serql_member_statement(C, [rdf(B, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#hasInstance', A)])), solutions(unsorted, inf, 0)), C, rdfs_entailment)).

