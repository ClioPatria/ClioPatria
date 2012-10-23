/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2006, University of Amsterdam

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

:- module(jena_functions, []).
:- use_module(library('semweb/rdf_db')).
:- use_module(sparql_runtime).

/** <module> Jena function library

@see http://jena.sourceforge.net/ARQ/library-function.html
*/

:- multifile
	sparql:function/2,
	sparql:current_function/1.

ns(afn, 'http://jena.hpl.hp.com/ARQ/function#').
ns(fn,  'http://www.w3.org/2005/xpath-functions#').
ns(Prefix, URI) :-
	rdf_current_ns(Prefix, URI).

alias('java:com.hp.hpl.jena.sparql.function.library.',
      'http://jena.hpl.hp.com/ARQ/function#').
alias('java:com.hp.hpl.jena.query.function.library.',
      'http://jena.hpl.hp.com/ARQ/function#').
alias('java:com.hp.hpl.jena.sparql.function.library.',
      'http://www.w3.org/2005/xpath-functions#').
alias('java:com.hp.hpl.jena.query.function.library.',
      'http://www.w3.org/2005/xpath-functions#').

function_alias(Prefix:Local, Args, Term) :-
	ns(Prefix, URI),
	alias(AliasBase, URI),
	atom_concat(AliasBase, Local, Name),
	Term =.. [Name|Args].

absolute_uri(Prefix:Local, Global) :-
	ns(Prefix, URI),
	atom_concat(URI, Local, Global).

%%	expand_body(+BodyIn, -BodyOut) is det.
%
%	Allow writing simple aliases using abbreviated notation.

expand_body(sparql:function(NS:Term0, R),
	    sparql:function(Term, R)) :- !,
	Term0 =.. [Name|Args],
	absolute_uri(NS:Name, P),
	Term =.. [P|Args].
expand_body(Body, Body).

term_expansion((sparql:function(NS:Term0, Result) :- Body0),
	       [ (sparql:function(Term, Result) :- Body),
		 sparql:current_function(Term)
	       | Aliases
	       ]) :-
	expand_body(Body0, Body),
	Term0 =.. [Name|Args],
	absolute_uri(NS:Name, P),
	Term =.. [P|Args],
	findall([ (sparql:(function(Alias, R) :- function(Term, R))),
		  (sparql:current_function(Alias))
		],
		function_alias(NS:Name, Args, Alias),
		Aliases).


		 /*******************************
		 *    XQUERY/XPATH FUNCTIONS	*
		 *******************************/

sparql:function(fn:'string-length'(S), L) :-
	sparql:function(afn:strlen(S), L).
sparql:function(fn:'lower-case'(literal(S)), literal(L)) :-
	lowercase_literal(S, L).
sparql:function(fn:'upper-case'(literal(S)), literal(L)) :-
	uppercase_literal(S, L).
sparql:function(fn:substring(String, Begin),
		literal(SubString)) :-
	sparql_eval(str(String), literal(In)),
	sparql_eval_raw(Begin, numeric(_Type, Value)),
	integer(Value),
	Start is Value+1,
	sub_atom(In, Start, _, 0, SubString).
sparql:function(fn:substring(String, Begin, Len),
		literal(SubString)) :-
	sparql_eval(str(String), literal(In)),
	sparql_eval_raw(Begin, numeric(_TypeB, B)),
	integer(B),
	sparql_eval_raw(Len, numeric(_TypeL, L)),
	integer(L),
	Start is B+1,
	sub_atom(In, Start, L, 0, SubString).

lowercase_literal(lang(Lang, Text), lang(Lang, Lower)) :-
	downcase_atom(Text, Lower).
lowercase_literal(Text, Lower) :-
	atom(Text),
	downcase_atom(Text, Lower).

uppercase_literal(lang(Lang, Text), lang(Lang, Lower)) :-
	upcase_atom(Text, Lower).
uppercase_literal(Text, Lower) :-
	atom(Text),
	upcase_atom(Text, Lower).



		 /*******************************
		 *	   ARQ FUNCTIONS	*
		 *******************************/

sparql:function(afn:strlen(A),
		literal(type(Type, LA))) :-
	sparql_eval(str(A), literal(A1)),
	rdf_equal(xsd:integer, Type),
	atom_length(A1, L),
	atom_number(LA, L).
sparql:function(afn:substr(String, Begin), Result) :-
	sparql:function(afn:substring(String, Begin), Result).
sparql:function(afn:substr(String, Begin, End), Result) :-
	sparql:function(afn:substring(String, Begin, End), Result).
sparql:function(afn:substring(String, Begin),
		literal(SubString)) :-
	sparql_eval(str(String), literal(In)),
	sparql_eval_raw(Begin, numeric(_Type, Start)),
	integer(Start),
	sub_atom(In, Start, _, 0, SubString).
sparql:function(afn:substring(String, Begin, End),
		literal(SubString)) :-
	sparql_eval(str(String), literal(In)),
	sparql_eval_raw(Begin, numeric(_TypeF, From)),
	integer(From),
	sparql_eval_raw(End, numeric(_TypeT, To)),
	integer(To),
	Len is To-From,
	sub_atom(In, From, Len, 0, SubString).
sparql:function(afn:sha1sum(S), literal(HA)) :-
	rdf_atom(S, Atom),
	sha_hash(Atom, Hash, []),
	hash_atom(Hash, HA).

rdf_atom(URI, URI) :-
	atom(URI).
rdf_atom(literal(lang(Lang, Text)), Atom) :-
	atomic_list_concat([Text, @, Lang], Atom).
rdf_atom(literal(lang(Type, Text)), Atom) :-
	atomic_list_concat([Text, ^^, Type], Atom).
rdf_atom(literal(Text), Text).

sparql:function(afn:localname(URI), literal(LN)) :-
	atom(URI),
	break_at(Break),
	sub_atom(URI, _, _, A, Break),
	sub_atom(URI, _, A, 0, LN),
	\+ sub_atom(LN, _, _, _, Break), !.
sparql:function(afn:namespace(URI), literal(NameSpace)) :-
	atom(URI),
	break_at(Break),
	sub_atom(URI, B, _, A, Break),
	sub_atom(URI, _, A, 0, LN),
	\+ sub_atom(LN, _, _, _, Break), !,
	NSLen is B + 1,
	sub_atom(URI, 0, NSLen, _, NameSpace).

break_at(#).
break_at(/).
