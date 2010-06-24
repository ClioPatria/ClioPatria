/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam,
		   VU University Amsterdam

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

:- module(cp_label,
	  [ turtle_label//1,		% +Literal
	    label_property/1,		% ?Property
	    text_of_literal/2		% +Literal, -Atom
	  ]).
:- use_module(library(error)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/html_write)).

:- use_module(cliopatria(hooks)).

/** <module> Support for showing labels
*/


%%	turtle_label(+RDFTerm)// is det.
%
%	HTML  rule  to  emit  an  RDF   term  (resource  or  object)  in
%	turtle-like notation with CSS classes.
%
%	@tbd	Implement possibility for a summary.

turtle_label(R) -->
	{ atom(R),
	  rdf_global_id(NS:Local, R), !
	},
	html(['<', span(class(ns), NS), ':', span(class(local), Local), '>']).
turtle_label(R) -->
	{ atom(R),
	  label_property(P),
	  rdf_has(R, P, Value),
	  text_of_literal(Value, Label), !
	},
	html(Label).
turtle_label(R) -->
	{ rdf_is_bnode(R) },
	bnode_label(R), !.
turtle_label(R) -->
	{ atom(R) }, !,
	html(R).
turtle_label(literal(Lit)) --> !,
	literal_label(Lit).


literal_label(type(Type, Value)) --> !,
	html(span(class(literal),
		  ['"', span(class(ltext), Value),
		   '"', span(class(tqual), '^^'), \turtle_label(Type)])).
literal_label(lang(Lang, Value)) --> !,
	html(span(class(literal),
		  ['"', span(class(ltext), Value), '"',
		   span(class(lqual), '@'), span(class(lang), Lang)])).
literal_label(Value) -->
	html(span(class(literal),
		  ['"', span(class(ltext), Value), '"'])).


%%	bnode_label(+Resource)// is semidet.
%
%	Display a bnode in a sensible way.
%
%	@tbd Provide a hook to customise this.

bnode_label(R) -->
	{ rdf(R, rdf:value, Value),
	  (   Value = literal(_)
	  ;   \+ rdf_is_bnode(Value)
	  )
	}, !,
	html(['[', \turtle_label(Value), '...]']).


%%	label_property(?Property) is nondet.
%
%	True if the value of  Property   can  be  used to (non-uniquely)
%	describe an object to the user.

:- rdf_meta
	label_property(r).

label_property(P) :-
	cliopatria:label_property(P).
label_property(rdfs:label).
label_property(skos:prefLabel).
label_property(skos:altLabel).
label_property(dc:title).

%%	text_of_literal(+Literal, -Text:atom) is det.
%
%	Extract the plain text from a literal.

text_of_literal(X, _) :-
	var(X), !,
	instantiation_error(X).
text_of_literal(literal(L), Text) :- !,
	text_of_literal(L, Text).
text_of_literal(type(Type, Value), Text) :- !,
	typed_text(Type, Value, Text).
text_of_literal(lang(_, Text), Text) :- !.
text_of_literal(Text, Text).

:- rdf_meta
	typed_text(r, +, -).

typed_text(_, Value, Text) :-
	atom(Value), !,
	Text = Value.
typed_text(rdfs:'XMLLiteral', Value, Text) :-
	xml_is_dom(Value), !,
	with_output_to(atom(Text),
		       xml_write(current_output, Value,
				 [ header(false),
				   layout(false)
				 ])).
typed_text(_, Value, Text) :-
	format(atom(Text), '~w', [Value]).

