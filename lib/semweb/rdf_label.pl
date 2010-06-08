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
	  [ rdf_label/2,
	    literal_text/2,		% +Literal, -Text
	    truncate_atom/3		% +Atom, -MaxLen -Text
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
	label_property/1.		% ?Resource

:- rdf_meta
	rdf_label(r, -),
	rdf_html_label(r, ?, ?),
	label_property(r).

label_property(rdfs:label).
label_property(skos:prefLabel).
label_property(dc:title).
label_property(skos:altLabel).


%%	rdf_label(+R, -Label:literal) is multi.
%
%	Label is a label for R.

rdf_label(R, Label) :-
	label_property(P),
	rdf_has(R, P, Label),
	rdf_is_literal(Label).


%%	literal_text(+Object, -Text:atom) is semidet.
%
%	Text is the textual content of Object. Fails if Object is not an
%	RDF literal (term literal(Value)). If   Object is an XMLLiteral,
%	Text is bound to the XML-text.
%
%	@error instantiation_error if Object is not ground

literal_text(X,_) :-
	\+ ground(X), !,
	instantiation_error(X).
literal_text(literal(X), Text) :- !,
	plain_text(X, Text).

plain_text(type(Type, X), Text) :- !,
	(   rdf_equal(Type, rdf:'XMLLiteral')
	->  with_output_to(atom(Text),
			   xml_write(current_output, X,
				     [ header(false),
				       layout(false)
				     ]))
	;   atomic(X)
	->  Text = X
	;   format(atom(Text), '~q', [X])
	).
plain_text(lang(_, Text), Text) :- !.
plain_text(X, Text) :-
	(   atomic(X)
	->  Text = X
	;   format(atom(Text), '~q', [X])
	).


%%	truncate_atom(+Atom, +MaxLen, -Truncated) is det.
%
%	If Atom is longer than MaxLen, truncate  it. If MaxLen is =inf=,
%	Truncated is simply unified with Atom.

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
