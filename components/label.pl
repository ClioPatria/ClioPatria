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
	    rdf_link//1,		% +RDFTerm
	    rdf_link//2			% +RDFTerm, +Options
	  ]).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(library(aggregate)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

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
	  literal_text(Value, Label), !
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


%%	rdf_link(+URI) is det.
%%	rdf_link(+URI, +Options) is det.
%
%	Make a hyper-link to an arbitrary   RDF resource or object using
%	the label.  Options processed:
%
%	    * resource_format(+Format)
%	    Determines peference for displaying resources.  Values are:
%
%	        * plain
%	        Display full resource a plain text
%	        * label
%	        Try to display a resource using its label
%	        * nslabel
%	        Try to display a resource as <prefix>:<Label>
%
%	This predicate creates two types of  links. Resources are linked
%	to the handler implementing   =list_resource= using r=<resource>
%	and  literals  that  appear  multiple    times   are  linked  to
%	=list_triples_with_object= using a Prolog  representation of the
%	literal.
%
%	@tbd	Make it easier to determine the format of the label
%	@tbd	Allow linking to different handlers.

rdf_link(R) -->
	rdf_link(R, []).

rdf_link(R, Options) -->
	{ atom(R), !,
	  http_link_to_id(list_resource, [r=R], HREF),
	  (   rdf(R, _, _)
	  ->  Class = lres
	  ;   Class = undef
	  )
	},
	html(a([class(Class), href(HREF)], \resource_label(R, Options))).
rdf_link(Literal, Options) -->
	{ (   option(graph(Graph), Options)
	  ->  aggregate_all(count, rdf(_,_,Literal, Graph), Count)
	  ;   aggregate_all(count, rdf(_,_,Literal), Count)
	  ),
	  Count > 1, !,
	  format(string(Title), 'Used ~D times', [Count]),
	  term_to_atom(Literal, Atom),
	  http_link_to_id(list_triples_with_object, [l=Atom], HREF)
	},
	html(a([ class(llobject),
		 href(HREF),
		 title(Title)
	       ],
	       \turtle_label(Literal))).
rdf_link(Literal, _) -->
	turtle_label(Literal).

resource_label(R, Options) -->
	{ option(resource_format(Format), Options) }, !,
	resource_flabel(Format, R).
resource_label(R, _) -->
	turtle_label(R).

resource_flabel(plain, R) --> !,
	html(R).
resource_flabel(label, R) --> !,
	(   { rdf_label(R, Literal), !,
	      literal_text(Literal, Label)
	    }
	->  html([span(class(rlabel),Label)])
	;   turtle_label(R)
	).
resource_flabel(nslabel, R) --> !,
	(   { rdf_global_id(NS:_Local, R), !,
	      label_property(P),
	      rdf_has(R, P, Value),
	      literal_text(Value, Label)
	    }
	->  html([span(class(ns),NS),':',span(class(rlabel),Label)])
	;   turtle_label(R)
	).
resource_flabel(_, R) -->
	turtle_label(R).
