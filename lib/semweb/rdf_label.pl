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

:- module(rdf_label,
	  [ rdf_label/2,		% +Resource, -Literal
	    rdf_display_label/3,	% +Resource, +Lang, -RDFOject
	    literal_text/2,		% +Literal, -Text
	    truncate_atom/3,		% +Atom, -MaxLen -Text
	    label_property/1		% ?Property
	  ]).
:- use_module(library(error)).
:- use_module(library(sgml_write)).
:- use_module(library(semweb/rdf_db)).


/** <module> Generate labels for RDF objects

This library deals with  a  common   problem  in  RDF applications: show
labels for resources and display literals.  There is no clear-cut answer
to this problem because there  are  too   many  options.  Think of e.g.,
language  preferences,  producing   summaries,    desired   rdfs/owl/...
reasoning. Therefore, this library provides the  required APIs a default
implementation and hooks that allow for dealing with the above mentioned
issues.
*/

:- multifile
	label_property/1,		% ?Resource
	label_hook/2,			% +Resource, -Literal
	display_label_hook/3.		% +Resource, ?Lang, -Label

:- rdf_meta
	rdf_label(r,-),
	rdf_display_label(r,?,-),
	label_property(r).

%%	label_property(?Property) is nondet.
%
%	True if Property is  used  to   represent  labels.  The  default
%	definition defines SKOS (prefLabel,  altLabel,   DC  (title) and
%	rdfs:label. This predicate is defined as =multifile=.

label_property(skos:prefLabel).
label_property(dc:title).
label_property(skos:altLabel).
label_property(rdfs:label).


%%	rdf_label(+R, -Label:literal) is nondet.
%
%	Label is a label for R.  This   predicate  first  calls the hook
%	label_hook/2. If this hook fails it produces all property-values
%	for the properties  defined  by   label_property/1  that  have a
%	literal value.

rdf_label(R, Label) :-
	(   label_hook(R, Label)
	*-> true
	;   label_property(P),
	    rdf_has(R, P, Label),
	    rdf_is_literal(Label)
	).


%%	rdf_display_label(+R, ?Lang, -Label:atom) is det.
%
%	Label is the preferred label to display the resource R in
%	the language Lang.

rdf_display_label(R, Lang, Label) :-
	display_label_hook(R, Lang, Label), !.
rdf_display_label(R, Lang, Label) :-
	(   nonvar(Lang)
	->  rdf_label(R, Literal),
	    Literal = literal(lang(L, _)),
	    lang_matches(L, Lang), !,
	    literal_text(Literal, Label)
	;   rdf_label(R, Literal),
	    literal_lang(Literal, Lang),
	    literal_text(Literal, Label), !
	).
rdf_display_label(BNode, Lang, Label) :-
	rdf_is_bnode(BNode),
	rdf_has(BNode, rdf:value, Value), !,
	rdf_display_label(Value, Lang, Label0),
	format(atom(Label), '[~a..]', Label0).
rdf_display_label(Resource, _, Label) :-
	(   after_char(Resource, '#', Local)
	->  Label = Local
	;   after_char(Resource, '/', Local)
	->  Label = Local
	;   Label = Resource
	).

after_char(Atom, Char, Rest) :-
	State = last(-),
	(   sub_atom(Atom, _, _, L, Char),
	    nb_setarg(1, State, L),
	    fail
	;   arg(1, State, L),
	    L \== (-)
	),
	sub_atom(Atom, _, L, 0, Rest).

literal_lang(literal(Lang, _), Lang) :- !.
literal_lang(_, _).

%%	literal_text(+Object, -Text:atom) is semidet.
%
%	Text is the textual content of Object. Fails if Object is not an
%	RDF literal (term literal(Value)). If   Object is an XMLLiteral,
%	Text is unified with the XML-text.
%
%	@error instantiation_error if Object is not ground

literal_text(X, _) :-
	\+ ground(X), !, !,
	instantiation_error(X).
literal_text(literal(L), Text) :- !,
	literal_text(L, Text).
literal_text(type(Type, Value), Text) :- !,
	typed_text(Type, Value, Text).
literal_text(lang(_, Text), Text) :- !.
literal_text(Text, Text).

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

%%	truncate_atom(+Atom, +MaxLen, -Truncated) is det.
%
%	If Atom is longer than MaxLen, truncate  it. If MaxLen is =inf=,
%	Truncated is unified with Atom.

truncate_atom(Atom, inf, All) :- !,
	All = Atom.
truncate_atom(Atom, MaxLen, Truncated) :-
	atom_length(Atom, Len),
	(   Len =< MaxLen
	->  Truncated = Atom
	;   TLen is max(3, MaxLen-4),
	    sub_atom(Atom, 0, TLen, _, S0),
	    atom_concat(S0, ' ...', Truncated)
	).
