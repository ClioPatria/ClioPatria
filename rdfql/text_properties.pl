/*  Part of ClioPatria SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2018, VU University Amsterdam
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

:- module(text_properties, []).
:- use_module(library(dcg/basics)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_litindex)).

:- multifile
    sparql:functional_property/2,
    sparql:current_functional_property/3.

:- rdf_register_prefix(tpf, 'http://cliopatria.swi-prolog.org/pf/text#').

term_expansion((sparql:functional_property(S, NS:Term0) :- Body),
               [ (sparql:functional_property(S, Term) :- Body),
                 sparql:current_functional_property(P, P, Argc)
               ]) :-
    Term0 =.. [Name|Args],
    length(Args, Argc),
    rdf_global_id(NS:Name, P),
    Term =.. [P|Args].


                 /*******************************
                 *       PROPERTY FUNCTIONS     *
                 *******************************/

%!  sparql:functional_property(+Subject, +Term) is nondet.
%
%   These functional properties  deal  with   text  matching.  As we
%   cannot refer to literals, the function  both accepts a predicate
%   and match.

sparql:functional_property(S, tpf:match(P, Pattern)) :-
    compile_pattern(Pattern, Spec, LV),
    search(S, P, Spec, LV).
sparql:functional_property(S, tpf:match(P, Pattern, literal(Found))) :-
    compile_pattern(Pattern, Spec, Found),
    search(S, P, Spec, Found).

search(S, P, prefix0(Prefix), Literal) :-
    !,
    rdf_has(S, P, literal(prefix(Prefix), Literal)).
search(S, P, Pattern, Literal) :-
    rdf_find_literal(Pattern, Plain),
    rdf_has(S, P, literal(exact(Plain), Literal)).

compile_pattern(Literal, Spec, LitValue) :-
    text_of(Literal, Pattern, Lang, LitValue),
    atom_codes(Pattern, PatternCodes),
    phrase(compile_pattern(Spec, Lang), PatternCodes).

text_of(literal(lang(Lang, Pattern)), Pattern, Lang, lang(Lang,_)) :- !.
text_of(literal(Pattern), Pattern, _, _).

compile_pattern(Spec, _) -->
    "^", !, rest(Prefix),
    { Spec = prefix0(Prefix) }.
compile_pattern(Spec, Lang) -->
    blanks, "(", compile_pattern(Spec, Lang), ")",
    !.
compile_pattern(Spec, Lang) -->
    blanks,
    simple_pattern(Left, Lang),
    (   and
    ->  compile_pattern(Right, Lang),
        { Spec = and(Left,Right) }
    ;   or
    ->  compile_pattern(Right, Lang),
        { Spec = or(Left,Right) }
    ;   compile_pattern(Right, Lang)
    ->  { Spec = and(Left,Right) }
    ;   blanks
    ->  { Spec = Left }
    ).

and --> blanks, "AND", blank, !, blanks.
or  --> blanks, "OR",  blank, !, blanks.

simple_pattern(not(Spec), Lang) -->
    "-",
    !,
    token_pattern(Spec, Lang).
simple_pattern(Spec, Lang) -->
    token_pattern(Spec, Lang).

token_pattern(Spec, Lang) -->
    word(Word),
    !,
    modifiers(Word, Lang, Spec).
token_pattern(Spec, _) -->
    number(N1),
    (   "..",
        number(N2)
    ->  { Spec = between(N1, N2) }
    ;   { Spec = N1 }
    ).
token_pattern(ge(N), _) -->
    ">=", number(N),
    !.
token_pattern(le(N), _) -->
    "<=", number(N),
    !.
token_pattern(le(N), _) -->
    "=<", number(N),
    !.

modifiers(Word, _, case(Word))   --> "/i", !.
modifiers(Word, _, prefix(Word)) --> "*", !.
modifiers(Word, L, stem(Word,L)) --> "/s", {nonvar(L)}, !.
modifiers(Word, _, stem(Word))   --> "/s", !.
modifiers(Word, _, sounds(Word)) --> "/S", !.
modifiers(Word, _, Word)         --> "".


%!  word(-Word)// is semidet.
%
%   Get a sequence of alpha chars as an atom

word(Word) -->
    alpha(First),
    alphas(Alphas),
    { atom_codes(Word, [First|Alphas])
    }.

alphas([H|T]) -->
    alpha(H),
    !,
    alphas(T).
alphas([]) --> [].

alpha(H) -->
    [H],
    { code_type(H, alpha) }.

rest(Atom, Codes, []) :-
    atom_codes(Atom, Codes).
