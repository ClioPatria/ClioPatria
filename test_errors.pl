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

test :-
	forall(e(N, _), test(N)).

test(N) :-
	e(N, Query),
	format('~`=t~72|~n'),
	format('Query: ~w~n', [Query]),
	format('~`=t~72|~n'),
	catch(serql_compile(Query, _, []), E, print_message(error, E)).

e(1, 'select x y from').
e(2, 'select x from {x}').
e(3, 'select x from {x} <rdfs:label>').
e(4, 'select x from {x} <rdfs:label> {vp:Subject} using namespace vp <foo>').
e(5, 'select x from {x} <rdfs:label> {vp:Subject} using namespace vp = <foo').
e(6, 'select x from {x} <rdfs:label> {vp:Subject} using namespace vp = <http://foo').
