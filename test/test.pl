/*  Part of ClioPatria

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cwi.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2010, VU University Amsterdam
		              CWI, Amsterdam
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

:- use_module(serql).

query(s1, 'select * from {S} P {O}').
query(s2, 'select * from {S} P {O1, O2}').
query(s3, 'select * from {S} P {O1};
 \t\t\t     Q {O2}').
query(s4, 'select * from {S} P1 {O1} P2 {O2}').
                                        % fig 5.5
query(s5, 'select * from {first} pred {} pred1 {obj1};
\t\t\t\t \t pred2 {obj2} pred3 {obj3}').
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
%%      numbervars(Query, 0, _, [singleton(true)]),
%%      format('ok(~q, ~W).~n', [Q, Query, [numbervars(true), quoted(true)]]),
        (   ok(Q, Reply)
    ->  (   Reply =@= Query
        ->  true
        ;   format(user_error, 'ERROR: failed on ~w~n', [Q])
        )
    ;   name_vars(Query),
        numbervars(Query, 0, _),
        format('ok(~q, ~p).~n', [Q, Query])
    ).

name_vars(select(Row, Names, _Path, _Where, _Limit, _Offset)) :-
    !,
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
    char_type(First, upper),
    !.
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

