/*  Part of ClioPatria SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015 VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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
		 *       PROPERTY FUNCTIONS	*
		 *******************************/

%%	sparql:functional_property(+Subject, +Term) is nondet.
%
%	These functional properties  deal  with   text  matching.  As we
%	cannot refer to literals, the function  both accepts a predicate
%	and match.

sparql:functional_property(S, tpf:match(P, Pattern)) :-
	compile_pattern(Pattern, Spec, LV),
	search(S, P, Spec, LV).
sparql:functional_property(S, tpf:match(P, Pattern, literal(Found))) :-
	compile_pattern(Pattern, Spec, Found),
	search(S, P, Spec, Found).

search(S, P, prefix0(Prefix), Literal) :- !,
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
	blanks, "(", compile_pattern(Spec, Lang), ")", !.
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
	"-", !,
	token_pattern(Spec, Lang).
simple_pattern(Spec, Lang) -->
	token_pattern(Spec, Lang).

token_pattern(Spec, Lang) -->
	word(Word), !,
	modifiers(Word, Lang, Spec).
token_pattern(Spec, _) -->
	number(N1),
	(   "..",
	    number(N2)
	->  { Spec = between(N1, N2) }
	;   { Spec = N1 }
	).
token_pattern(ge(N), _) -->
	">=", number(N), !.
token_pattern(le(N), _) -->
	"<=", number(N), !.
token_pattern(le(N), _) -->
	"=<", number(N), !.

modifiers(Word, _, case(Word))   --> "/i", !.
modifiers(Word, _, prefix(Word)) --> "*", !.
modifiers(Word, L, stem(Word,L)) --> "/s", {nonvar(L)}, !.
modifiers(Word, _, stem(Word))   --> "/s", !.
modifiers(Word, _, sounds(Word)) --> "/S", !.
modifiers(Word, _, Word)	 --> "".


%%	word(-Word)// is semidet.
%
%	Get a sequence of alpha chars as an atom

word(Word) -->
	alpha(First),
	alphas(Alphas),
	{ atom_codes(Word, [First|Alphas])
	}.

alphas([H|T]) -->
	alpha(H), !,
	alphas(T).
alphas([]) --> [].

alpha(H) -->
	[H],
	{ code_type(H, alpha) }.

rest(Atom, Codes, []) :-
	atom_codes(Atom, Codes).
