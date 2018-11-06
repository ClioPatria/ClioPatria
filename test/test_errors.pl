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
