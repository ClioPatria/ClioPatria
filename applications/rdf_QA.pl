/*  This file is part of ClioPatria.

    Author:	Jan Wielemaker <wielemak@science.uva.nl>
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

:- module(rdf_qa,
	  [
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library('http/html_head')).
:- use_module(library(url)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(util(rdf_util)).
:- use_module(util(util)).
:- use_module(util(html_util)).
:- use_module(timespace(timeparse)).

% gui components
:- use_module(components(container)).
:- use_module(components(page)).

:- include(server(hooks)).

/** <module> Simple RDF Quality Assurance tests

This module defines a web-frontend for displaying problems with the RDF
in our database.  The tests are implemented by rdf_warning/2.  Provided
HTTP locations are:

	$ /qa_index :
	Give a list of the tests provided after counting the number of
	problems upto a maximum of 100.

	$ /qa :
	Give the actual report.  Accepts the following parameters:

	    * max_per_ns
	    Maximum number of warnings per namespace (20).

	    * class
	    If present, limit output to warnings of given class
	    (see rdf_warning/2)

	    * ns
	    If present, limit output to this namespace.  Disables
	    =max_per_ns=.

	    * show
	    How to display conflicting URIs.  Default is =local_view=,
	    creating a link to the local view.  =uri= shows the plain
	    URI as a link.
*/

:- http_handler(session('qa_index'), rdf_qa_index, [content_type(text/html)]).
:- http_handler(session('qa'),	     rdf_qa,	   [content_type(text/html)]).

:- html_resource(qa,
		 [ virtual(true),
		   requires(skins('qa.css'))
		 ]).

%%	rdf_qa_index(+Request)
%
%	Show list of available RDF quality tests.

rdf_qa_index(_Request) :-
	findall(Class, clause(rdf_warning(Class, _), _), Classes),
	application_page([
			  grid(doc3),
			  title('Generate RDF quality reports'),
			  body(\qa_index(Classes))
			 ]).

qa_index([]) --> [].
qa_index([Class|T]) -->
	{ count_unique(URI, rdf_warning(Class, URI), 100, C),
	  C > 0, !,
	  http_location([ path(qa),
			  search([ class=Class ])
			], Location)
	},
	html_requires(qa),
	html(div(class(qa_class_title), a(href(Location), [\class_label(Class), \count(100, C)]))),
	qa_index(T).
qa_index([_|T]) -->
	qa_index(T).

count(C, C) -->
	html(' (> ~D)'-[C]).
count(_, C) -->
	html(' (~D)'-[C]).


%%	rdf_qa(Request)
%
%	Display quality issues with the loaded RDF.

rdf_qa(Request) :-
	http_parameters(Request,
			[ max_per_ns(Max0, [integer, default(20)]),
			  class(Class, [optional(true)]),
			  ns(NS, [optional(true)]),
			  show(Show, [oneof([local_view,uri]),
				      default(local_view)])
			]),
	(   nonvar(NS)
	->  Max = inf
	;   Max = Max0
	),
	include(ground, [ns(NS), max_per_ns(Max), show(Show)], Options),
	findall(Class, clause(rdf_warning(Class, _), _), Classes),
	warnings_by_class(Classes, ByCLass, Options),
	application_page([title('RDF Quality report'),
			  grid(doc3),
			  body(\qa_report(ByCLass, Options))
			]).


		 /*******************************
		 *	     COLLECT		*
		 *******************************/

warnings_by_class([], [], _).
warnings_by_class([H|T0], [H-Warnings|T], Options) :-
	warnings_for_class(H, Warnings, Options),
	Warnings \== [], !,
	warnings_by_class(T0, T, Options).
warnings_by_class([_|T0], T, Options) :-
	warnings_by_class(T0, T, Options).

warnings_for_class(Class, Grouped, Options) :-
	option(max_per_ns(Max), Options, 20),
	option(ns(NS), Options, _),
	find_unique_pairs(NS-URI, warning_by_ns(Class, NS, URI),
			  inf, Max, Grouped).

warning_by_ns(Warning, NS, URI) :-
	rdf_warning(Warning, URI),
	namespace(URI, NS).

namespace(rdf(URI, _, _), NS) :- !,
	rdf_split_url(NS, _Id, URI).
namespace(URI, NS) :- !,
	rdf_split_url(NS, _Id, URI).

%%	rdf_warning(+Class, -URI) is nondet.
%
%	True if we should give warning Class about URI. Class must be an
%	atom and appear  directly  in  the   head  as  the  classes  are
%	enumerated using clause/2. Also add a clause to class_label//1.

rdf_warning(no_label, URI) :-
	rdf_subject(URI),
	\+ cliopatria:rdf_qa_ok(no_label, URI),
	\+ rdf_is_bnode(URI),
	\+ rdf_has(URI, rdfs:label, _),
	\+ sub_atom(URI, 0, _, _, 'http://www.w3.org/2001/XMLSchema#'),
	\+ rdfs_individual_of(URI, vra:'Image'),
	\+ rdfs_individual_of(URI, vra:'Work').
rdf_warning(no_type, URI) :-
	rdf_subject(URI),
	\+ rdf_has(URI, rdf:type, _),
	\+ rdf(_, rdfs:isDefinedBy, URI),   % Schema URI
	\+ rdf_has(URI, rdf:first, _).		% Nodes in RDF collections.
rdf_warning(undefined_object, URI) :-
	rdf(_,_,URI), atom(URI),
	\+ rdf(URI, _, _),
	\+ rdf(_,rdfs:seeAlso, URI), % Most likely a schema or doc uri
	\+ rdf(_, rdfs:isDefinedBy, URI),
	\+ rdf(_, owl:imports, URI),
	\+ rdf(_, owl:priorVersion, URI),
	\+ rdf(_, dcterms:hasVersion, URI),
	\+ sub_atom(URI, 0, _, _, 'http://www.w3.org/2001/XMLSchema#').
rdf_warning(cycle_property, rdf(S,P,S)) :-
	rdf(S, P, S),
	\+ (  rdf_equal(P, rdf:type),
	      rdf_equal(S, rdfs:'Class')
	   ).
rdf_warning(property_no_label_en, URI) :-
	rdf_warning(property_no_label, en, URI).
rdf_warning(property_no_label_nl, URI) :-
	rdf_warning(property_no_label, nl, URI).

rdf_warning(no_vra_property, P) :-
	retractall(dc_prop_cache(_,_)),
	rdfs_individual_of(URI, vra:'Work'),
	rdf(URI, P, _),
	\+ dc_property(P).
rdf_warning(created_while_not_active, Work) :-
	rdfs_individual_of(Work, vra:'Work'),
	rdf_has(Work, vra:'date.creation', Created),
	to_year(Created, YCreated),
	rdf_has(Work, vra:creator, Artist),
	\+ (   (   rdf_has(Artist, iface:startDate, Born),
		   to_year(Born, YBorn)
	       ->  YCreated - YBorn >= 10
	       ;   true
	       ),
	       (   rdf_has(Artist, iface:endDate, Died),
		   to_year(Died, YDied)
	       ->  YDied >= YCreated
	       ;   true
	       )
	   ).

rdf_warning(property_no_label, Lang, URI) :-
	rdf_current_predicate(URI),
	\+ sub_atom(URI, 0, _ , _, 'http://e-culture.multimedian.nl/ns/interface/'),
	\+ (
	   rdf_has(URI, rdfs:label, literal(lang(Lang, _))),
	    nonvar(Lang),
	    sub_atom(Lang,0,_,_,'en')
	   ).

to_year(Object, Year) :-
	date_year(Object, YearOrRange),
	(   YearOrRange = range(Year, _)
	->  true
	;   Year = YearOrRange
	).


:- dynamic
	dc_prop_cache/2.

dc_property(P) :-
	dc_prop_cache(P, True), !,
	True == true.
dc_property(P) :-
	(   rdfs_subproperty_of(P, S),
	    root_property(_, S)
	->  assert(dc_prop_cache(P, true))
	;   assert(dc_prop_cache(P, false)),
	    fail
	).

term_expansion(root_property(Which, NSID),
	       root_property(Which, URI)) :-
	rdf_global_id(NSID, URI).

root_property(dc, dc:type).
root_property(dc, dc:title).
root_property(dc, dc:format).
root_property(dc, dc:creator).
root_property(dc, dc:date).
root_property(dc, dc:contributor).
root_property(dc, dc:identifier).
root_property(dc, dc:coverage).
root_property(dc, dc:subject).
root_property(dc, dc:relation).
root_property(dc, dc:description).
root_property(dc, dc:source).
root_property(dc, dc:rights).
root_property(rdf, rdf:type).
root_property(owl, owl:sameAs).


		 /*******************************
		 *	      PRESENT		*
		 *******************************/

class_label(no_label) -->
	html('URIs without label').
class_label(no_type) -->
	html('URIs without type').
class_label(undefined_object) -->
	html('URIs used as object without properties').
class_label(cycle_property) -->
	html(['Triples with ', i('Subject == Object')]).
class_label(property_no_label_en) -->
	html('Properties without label in English').
class_label(property_no_label_nl) -->
	html('Properties without label in Dutch').
class_label(no_vra_property) -->
	html('Properties that should not appear on vra:Work instances').
class_label(created_while_not_active) -->
	html('Works not created inside the artists lifespan').

qa_report([], _) --> !,
	html(p('Could not find any problems in the RDF.')).
qa_report(Classes, Options) -->
	html_requires(qa),
	html([ %ul(\index(Classes)),
	       \report_by_class(Classes, Options)
	     ]).

index([]) -->
	[].
index([Class-_|T]) -->
	html(li(class(index), a(href('#'+Class), \class_label(Class)))),
	index(T).


report_by_class([], _) -->
	[].
report_by_class([Class-Grouped|T], Options) -->
	html([ h3(class(qa_class_heading),a(name(Class), \class_label(Class))),
	       ul(\show_groups(Grouped, [class(Class)|Options]))
	     ]),
	report_by_class(T, Options).

show_groups([], _) -->
	[].
show_groups([NS-URIs|T], Options) -->
	html(li(class(show_groups_li), [ \show_namespace(NS, Options),
		  ol(\show_uris(URIs, [ns(NS)|Options]))
		])),
	show_groups(T, Options).

show_namespace(NS, Options) -->
	{ atom_concat('__file://', Path, NS), !,
	  option(class(Class), Options)
	},
	html([\class_label(Class), ' for blank nodes from ', tt(Path)]).
show_namespace(NS, Options) -->
	{ option(class(Class), Options)
	},
	html([\class_label(Class), ' for namespace ', tt(NS)]).

show_uris(URIs, Options) -->
	{ option(max_per_ns(Max), Options, 20),
	  option(show(Show), Options, local_view),
	  length(URIs, Len)
	},
	list_uris(URIs, Show),
	(   {Max == inf ; Len < Max}
	->  []
	;   more_link(Options)
	).

list_uris([], _) -->
	[].
list_uris([H|T], Show) -->
	show_hit(H, Show),
	list_uris(T, Show).

show_hit(H, Show) -->
	{ atom(H) }, !,
	show_uri(H, Show).
show_hit(H, Show) -->
	{ H = rdf(_,_,_) }, !,
	show_triple(H, Show).

show_uri(H, uri) --> !,
	html(li(class(show_uri), a(href(H), H))).
show_uri(H, _) -->
	html(li(class(show_uri_local), \local_view_link(H))).

show_triple(rdf(S,P,O), _) -->
	{ rdf(S,P,O,DB) }, !,
	html(li([ '{',
		  \local_view_link(S),
		  ', ',
		  \local_view_link(P),
		  ', ',
		  \local_view_link(O),
		  '}',
		  \source(DB)
		])).

source(URI:Line) -->
	{ Line < 1e9, !,
	  file_base_name(URI, Base)
	},
	html([' from ', code(Base), ' at line ~D'-[Line]]).
source(URI:Time) --> !,
	{ format_time(string(T), '%F:~R', Time) },
	html([' by ', code(URI), ' at ', T]).
source(X) -->				% debugging
	{ term_to_atom(X, A) },
	html(A).


more_link(Options) -->
	{ option(class(Class), Options),
	  option(ns(NS), Options),
	  http_location([ path(qa),
			  search([ ns=NS, class=Class ])
			], Location)
	},
	html(['... ', a(href(Location), 'show all')]).
