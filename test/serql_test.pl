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
		 *	     DATABASE		*
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
		 *	    TEST DRIVER		*
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
		 *	      QUERIES		*
		 *******************************/

q(1, 'select X from
	{X} <!label> {L} where label(L) like "label*"').
q(2, 'select L from
	{X} <!label> {L} where X = "r1"').
q(3, 'select L from 
	{X} <!label> {L} where X = "r1" or X = "r2"').
q(4, 'select L from
	{X} <!label> {L} where isliteral(L) and L like "label*"').
q(5, 'select A,B from
	{A} <!label> {X},
	{B} <!comment> {Y}').
q(6, 'select A,B from
	{A} <!type> {Y} <!label> {B}').


		 /*******************************
		 *	 GOLDEN STANDARD	*
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
