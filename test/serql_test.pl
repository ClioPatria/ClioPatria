/*  Part of ClioPatria

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cwi.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010, VU University Amsterdam
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Very limited test harnass, mainly  to   test  the  SeRQL optimiser. This
should be extended with many more queries   and  have some check whether
the optimised code produces reasonable results. Can we compute and check
the produced complexity?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- load_files([ library('semweb/rdf_db'),
                serql
              ],
              [ if(not_loaded),
                silent(true)
              ]).
:- use_module(rdf_entailment, []).

% :- debug(serql(compiled)).

                 /*******************************
                 *           DATABASE           *
                 *******************************/

db(r1, p1, r2).
db(r2, p1, r3).
db(r3, p1, r4).
db(r1, label, literal(label_one)).
db(r2, label, literal(label_two)).
db(r3, label, literal(label_three)).
db(r1, type, c1).
db(c1, label, literal(class_one)).
db(c1, comment, literal('Nice class')).

init_db :-
    rdf_reset_db,
    forall(db(S,P,O),
           rdf_assert(S,P,O)).


                 /*******************************
                 *          TEST DRIVER         *
                 *******************************/

:- dynamic failed/1.

test(N, Row) :-
    q(N, Q),
    serql_query(Q, Row).

test :-
    forall(q(N, _), test(N)),
    findall(N, failed(N), Failed),
    (   Failed == []
    ->  format('All tests passed~n')
    ;   format('Tests failed: ~p~n', [Failed])
    ).

test(N) :-
    init_db,
    retractall(failed(_)),
    q(N, Q),
    setof(Row, serql_query(Q, Row), Rows),
    (   ok(N, OKRows)
    ->  (   OKRows == Rows
        ->  true
        ;   format('FAILED: ~w~n', [N]),
            assert(failed(N))
        )
    ;   format('ok(~q, ~q).~n', [N, Rows])
    ).


                 /*******************************
                 *            QUERIES           *
                 *******************************/

q(1, 'select X from
\t{X} <!label> {L} where label(L) like "label*"').
q(2, 'select L from
\t{X} <!label> {L} where X = "r1"').
q(3, 'select L from
\t{X} <!label> {L} where X = "r1" or X = "r2"').
q(4, 'select L from
\t{X} <!label> {L} where isliteral(L) and L like "label*"').
q(5, 'select A,B from
\t{A} <!label> {X},
\t{B} <!comment> {Y}').
q(6, 'select A,B from
\t{A} <!type> {Y} <!label> {B}').


                 /*******************************
                 *       GOLDEN STANDARD        *
                 *******************************/

ok(_,_) :- fail.

ok(1, [row(r1),
       row(r2),
       row(r3)]).
ok(2, [row(literal(label_one))]).
ok(3, [row(literal(label_one)),
       row(literal(label_two))]).
ok(4, [row(literal(label_one)),
       row(literal(label_three)),
       row(literal(label_two))]).
ok(5, [row(c1, c1),
       row(r1, c1),
       row(r2, c1),
       row(r3, c1)]).
ok(6, [row(r1, literal(class_one))]).
