/*  This file is part of ClioPatria.

    Author:	Jan Wielemaker,,, <janw@hppc323.few.vu.nl>
    HTTP:	http://e-culture.multimedian.nl/software/ClioPatria.shtml
    GITWEB:	http://eculture.cs.vu.nl/git/ClioPatria.git
    GIT:	git://eculture.cs.vu.nl/home/git/eculture/ClioPatria.git
    GIT:	http://eculture.cs.vu.nl/home/git/eculture/ClioPatria.git
    Copyright:  2005-2009, E-Culture/MultimediaN

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

:- module(browse_named_graphs,
	  [ rdf_search_form//0,
	    resource_link//2		% +ResourceOrLiteral, +Options
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/yui_resources)).

:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_litindex)).

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(library(uri)).
:- use_module(library(pairs)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(apply)).

:- use_module(http_canviz).
:- use_module(library(semweb/rdf_abstract)).

:- use_module(http_user).
:- use_module(user_db).


		 /*******************************
		 *	      PATHS		*
		 *******************************/

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(rdf_browser, root(browse), []).

:- http_handler(rdf_browser(.),		      welcome,         [prefix]).
:- http_handler(rdf_browser(list_graphs),     list_graphs,     []).
:- http_handler(rdf_browser(list_graph),      list_graph,      []).
:- http_handler(rdf_browser(list_classes),    list_classes,    []).
:- http_handler(rdf_browser(list_instances),  list_instances,  []).
:- http_handler(rdf_browser(list_predicates), list_predicates, []).
:- http_handler(rdf_browser(list_predicate_resources),
					      list_predicate_resources, []).
:- http_handler(rdf_browser(list_resource),   list_resource,   []).
:- http_handler(rdf_browser(list_triples),    list_triples,    []).
:- http_handler(rdf_browser(list_triples_with_object),
					      list_triples_with_object,	[]).
:- http_handler(rdf_browser(delete_graph),    delete_graph,    []).
:- http_handler(rdf_browser(download_graph),  download_graph,  []).

:- http_handler(rdf_browser(ac_find),         ac_find,	       []).
:- http_handler(rdf_browser(search),          search,	       []).


		 /*******************************
		 *	     HANDLERS		*
		 *******************************/

%%	welcome(Request)
%
%	Entry page

welcome(_Request) :-
	reply_html_page(cliopatria(default),
			title('RDF Explorer'),
			[ h4('Welcome to the SWI-Prolog RDF Explorer'),
			  p([ 'This web-application allows the user to get an ',
			      'overview of an RDF database.'
			    ])
			]).


%%	list_graphs(+Request)
%
%	Display a page holding a table with all RDF graphs.  The graphs
%	are sorted to the number of triples.

list_graphs(_Request) :-
	findall(Count-Graph,
		(   rdf_graph(Graph),
		    rdf_statistics(triples_by_file(Graph, Count))
		),
		Pairs),
	keysort(Pairs, Sorted),
	pairs_values(Sorted, UpCount),
	reverse(UpCount, DownCount),
	append(DownCount, [virtual(total)], Rows),
	reply_html_page(cliopatria(default),
			title('RDF Graphs'),
			[ h4('Named graphs in the RDF store'),
			  \graph_table(Rows, [])
			]).


graph_table(Graphs, Options) -->
	{ option(top_max(TopMax), Options, 500),
	  option(top_max(BottomMax), Options, 500)
	},
	html(table(class(rdf_browse),
		   [ \graph_table_header
		   | \table_rows_top_bottom(graph_row, Graphs,
					    TopMax, BottomMax)
		   ])).

graph_table_header -->
	html(tr([ th('RDF Graph'),
		  th('Triples')
		])).

graph_row(virtual(total)) --> !,
	{ rdf_statistics(triples(Count))
	},
	html([ th(class(total), 'Total #triples:'),
	       td(class(int), '~D'-[Count])
	     ]).
graph_row(Graph) -->
	{ rdf_statistics(triples_by_file(Graph, Count))
	},
	html([ td(\graph_link(Graph)),
	       td(class(int), '~D'-[Count])
	     ]).

graph_link(Graph) -->
	{ http_link_to_id(list_graph, [graph=Graph], URI)
	},
	html(a(href(URI), Graph)).

%%	list_graph(+Request)
%
%	Provide information about an individual RDF graph

list_graph(Request) :-
	http_parameters(Request,
			[ graph(Graph, [])
			]),
	(   rdf_graph(Graph)
	->  true
	;   existence_error(graph, Graph)
	),
	reply_html_page(cliopatria(default),
			title('RDF Graph ~w'-[Graph]),
			[ h4('Summary information for graph "~w"'-[Graph]),
			  \graph_info(Graph),
			  \graph_actions(Graph)
			]).

graph_info(Graph) -->
	html_property_table(row(P,V),
			    graph_property(Graph,P,V)).

graph_property(Graph, source, Source) :-
	rdf_source(Graph, Source).
graph_property(Graph, triples, int(Triples)) :-
	rdf_statistics(triples_by_file(Graph, Triples)).
graph_property(Graph, predicate_count(Graph), int(Count)) :-
	aggregate_all(count, predicate_in_graph(Graph, _P), Count).
graph_property(Graph, subject_count(Graph), int(Count)) :-
	aggregate_all(count, subject_in_graph(Graph, _P), Count).
graph_property(Graph, bnode_count(Graph), int(Count)) :-
	aggregate_all(count, bnode_in_graph(Graph, _P), Count).
graph_property(Graph, type_count(Graph), int(Count)) :-
	aggregate_all(count, type_in_graph(Graph, _P), Count).

predicate_in_graph(Graph, P) :-
	rdf_current_predicate(P),
	once(rdf(_,P,_,Graph)).

%%	subject_in_graph(+Graph, -Subject)
%
%	Generate the distinct subjects in a graph. There are two ways to
%	do this: first the subjects and then  whether they appear in the
%	graph or the other way around. At   least this has the advantage
%	that we get distinct subjects for free.

subject_in_graph(Graph, S) :-
	rdf_subject(S),
	once(rdf(S, _, _, Graph)).

bnode_in_graph(Graph, S) :-
	rdf_subject(S),
	rdf_is_bnode(S),
	once(rdf(S, _, _, Graph)).

%%	type_in_graph(+Graph, -Class)
%
%	Generate the unique types in Graph

:- thread_local
	type_seen/1.

type_in_graph(Graph, Class) :-
	call_cleanup(type_in_graph2(Graph, Class),
		     retractall(type_seen(_))).

type_in_graph2(Graph, Class) :-
	subject_in_graph(Graph, S),
	(   rdf(S, rdf:type, Class)
	*-> true
	;   rdf_equal(Class, rdfs:'Resource')
	),
	(   type_seen(Class)
	->  fail
	;   assert(type_seen(Class))
	).


%%	graph_actions(+Graph)// is det.
%
%	Provide a form for basic actions on the graph

graph_actions(Graph) -->
	html(ul(class(graph_actions),
		[ \li_download_graph(Graph, show),
		  \li_download_graph(Graph, download),
		  \li_delete_graph(Graph)
		])).

li_delete_graph(Graph) -->
	{ logged_on(User, X),
	  X \== User,
	  catch(check_permission(User, write(_, unload(Graph))), _, fail), !,
	  http_link_to_id(unload_graph, [], Action)
	},
	html(li(form(action(Action),
		     [ input([type(hidden), name(graph), value(Graph)]),
		       input([type(hidden), name(resultFormat), value(html)]),
		       input([class(gaction), type(submit), value('Delete')]),
		       ' this graph'
		     ]))).
li_delete_graph(_) --> [].

li_download_graph(Graph, How) -->
	{ http_link_to_id(download_graph, [], Action),
	  download_options(How, Label, MimeType)
	},
	html(li(form(action(Action),
		     [ input([type(hidden), name(graph), value(Graph)]),
		       input([type(hidden), name(mimetype), value(MimeType)]),
		       input([class(gaction), type(submit), value(Label)]),
		       ' this graph as ',
		       \dl_format_menu
		     ]))).

download_options(show,     'Show',     'text/plain').
download_options(download, 'Download', default).

dl_format_menu -->
	html(select(name(format),
		    [ option([value(turtle),selected],  'Turtle'),
		      option([value(canonical_turtle)], 'Canonical Turtle'),
		      option([value(rdfxml)],           'RDF/XML')
		    ])).


%%	download_graph(+Request)
%
%	Download a graph.

download_graph(Request) :-
	http_parameters(Request,
			[ graph(Graph, []),
			  format(Format, [ oneof([turtle,
						  canonical_turtle,
						  rdfxml
						 ]),
					   default(turtle)
					 ]),
			  mimetype(Mime, [ default(default) ])
			]),
	send_graph(Graph, Format, Mime).

send_graph(Graph, Format, default) :- !,
	default_mime_type(Format, MimeType),
	send_graph(Graph, Format, MimeType).
send_graph(Graph, Format, MimeType) :- !,
	format('Transfer-Encoding: chunked~n'),
	format('Content-type: ~w; charset=UTF8~n~n', [MimeType]),
	send_graph(Graph, Format).

send_graph(Graph, turtle) :- !,
	rdf_save_turtle(stream(current_output), [graph(Graph)]).
send_graph(Graph, canonical_turtle) :- !,
	rdf_save_canonical_turtle(stream(current_output), [graph(Graph)]).
send_graph(Graph, rdfxml) :- !,
	rdf_save(stream(current_output), [graph(Graph)]).

default_mime_type(turtle, text/turtle).
default_mime_type(canonical_turtle, text/turtle).
default_mime_type(rdfxml, application/'rdf+xml').


%%	delete_graph(+Request)
%
%	Delete all triples from the given graph.

delete_graph(Request) :-
	http_parameters(Request,
			[ graph(Graph, [])
			]),
	rdf_statistics(triples_by_file(Graph, Triples)),
	rdf_retractall(_,_,_,Graph),
	reply_html_page(cliopatria(default),
			title('Deleted graph ~w'-[Graph]),
			p('Deleted graph ~w (~D triples)'-[Graph, Triples])).


%%	list_classes(+Request)
%
%	List all classes of a graph, sorted by label.

list_classes(Request) :-
	http_parameters(Request,
			[ graph(Graph, [])
			]),
	findall(Type, type_in_graph(Graph, Type), Types),
	sort_by_label(Types, Sorted),
	reply_html_page(cliopatria(default),
			title('Classes in graph ~w'-[Graph]),
			[ h4(['Classes in graph ', \graph_link(Graph)]),
			  \class_table(Sorted, Graph, [])
			]).

class_table(Classes, Graph, Options) -->
	{ option(top_max(TopMax), Options, 500),
	  option(top_max(BottomMax), Options, 500)
	},
	html(table(class(rdf_browse),
		   [ \class_table_header
		   | \table_rows_top_bottom(class_row(Graph), Classes,
					    TopMax, BottomMax)
		   ])).

class_table_header -->
	html(tr([ th('Class'),
		  th('#Instances'),
		  th('avg. properties')
		])).

class_row(Graph, Class) -->
	{ class_statistics(Graph, Class, InstanceCount, PropCount),
	  (   var(Graph)
	  ->  Params = [class(Class)]
	  ;   Params = [graph(Graph), class(Class)]
	  ),
	  http_link_to_id(list_instances, Params, ILink)
	},
	html([ td(\resource_link(Class)),
	       td(class(int), a(href(ILink), InstanceCount)),
	       td(class(int), \float_or_minus(PropCount))
	     ]).

float_or_minus(N) -->
	{ number(N) }, !,
	html(['~1f'-[N]]).
float_or_minus(X) --> !,
	html(X).

%%	class_statistics(+Graph, +Class, -InstanceCount, -PropAvg)
%
%	Gather statistics about the use of a class in a given Graph.

class_statistics(Graph, Class, InstanceCount, PropCount) :-
	aggregate_all(count-sum(C),
		      instance_in_graph(Graph, Class, _, C),
		      InstanceCount-PropSum),
	(   InstanceCount =:= 0
	->  PropCount = (-)
	;   PropCount is PropSum/InstanceCount
	).

%%	instance_in_graph(?Graph, ?Class, +Type,
%%			  -Subject, -PropertyCount) is nondet.
%
%	True of Subject is  an  instance   of  Class  with PropertyCount
%	properties provided from Graph.

instance_in_graph(Graph, Class, any, S, C) :- !,
	instance_in_graph(Graph, Class, S, C).
instance_in_graph(Graph, Class, bnode, S, C) :- !,
	freeze(S, rdf_is_bnode(S)),
	instance_in_graph(Graph, Class, S, C).


instance_in_graph(Graph, Class, S, C) :-
	var(Class), !,
	rdf_subject(S),
	once(rdf(S, _, _, Graph)),
	property_count(Graph, S, C).
instance_in_graph(Graph, Class, S, C) :-
	rdf_equal(Class, rdfs:'Resource'), !,
	(   rdf(S, rdf:type, Class),
	    once(rdf(S, _, _, Graph))
	;   subject_in_graph(Graph, S),
	    \+ rdf(S, rdf:type, _)
	),
	property_count(Graph, S, C).
instance_in_graph(Graph, Class, S, C) :-
	rdf(S, rdf:type, Class),
	once(rdf(S, _, _, Graph)),
	property_count(Graph, S, C).

property_count(Graph, S, Count) :-
	aggregate_all(count, rdf(S, _, _, Graph), Count).


		 /*******************************
		 *	  LIST INSTANCES	*
		 *******************************/

%%	list_instances(+Request)
%
%	List instances of a given class

list_instances(Request) :-
	http_parameters(Request,
			[ class(Class, [ optional(true)
				       ]),
			  graph(Graph, [ optional(true)
				       ]),
			  type(Type,   [ oneof([any, bnode]),
					 default(any)
				       ]),
			  sortBy(Sort, [ oneof([label,properties]),
					 default(label)
				       ])

			]),
	findall(I-PC, instance_in_graph(Graph, Class, Type, I, PC), IPairs),
	sort_pairs_by_label(IPairs, TableByName),
	(   Sort == properties
	->  reverse(TableByName, RevTableByName),
	    transpose_pairs(RevTableByName, FPairsUp),
	    reverse(FPairsUp, FPairsDown),
	    flip_pairs(FPairsDown, Table)
	;   Table = TableByName
	),

	reply_html_page(cliopatria(default),
			title(\instance_table_title(Graph, Class, Sort)),
			[ h4(\html_instance_table_title(Graph, Class, Sort)),
			  \instance_table(Table, [])
			]).

instance_table_title(Graph, Class, Sort) -->
	{ var(Class) }, !,
	html('Instances in ~w sorted by ~w'-
	     [Graph, Sort]).
instance_table_title(Graph, Class, Sort) -->
	{ label_of(Class, Label) },
	html('Instances of ~w in ~w sorted by ~w'-
	     [Label, Graph, Sort]).

html_instance_table_title(Graph, Class, Sort) -->
	html([ 'Instances',
	       \of_class(Class),
	       \in_graph(Graph),
	       \sorted_by(Sort)
	     ]).

of_class(Class) -->
	{ var(Class) }, !.
of_class(Class) -->
	html([' of class ', \resource_link(Class)]).

in_graph(Graph) -->
	{ var(Graph) }, !.
in_graph(Graph) -->
	html([' in graph ', \graph_link(Graph)]).

sorted_by(Sort) -->
	html(' sorted by ~w'-[Sort]).


instance_table(Pairs, Options) -->
	{ option(top_max(TopMax), Options, 500),
	  option(top_max(BottomMax), Options, 500)
	},
	html(table(class(rdf_browse),
		   [ \instance_table_header
		   | \table_rows_top_bottom(instance_row, Pairs,
					    TopMax, BottomMax)
		   ])).

instance_table_header -->
	html(tr([ th('Instance'),
		  th('#Properties')
		])).

instance_row(R-C) -->
	html([ td(\resource_link(R)),
	       td(class(int), C)
	     ]).


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

%%	list_predicates(+Request)
%
%	List all predicates used in graph, sorted by label.

list_predicates(Request) :-
	http_parameters(Request,
			[ graph(Graph, [])
			]),
	findall(Pred, predicate_in_graph(Graph, Pred), Preds),
	sort_by_label(Preds, Sorted),
	reply_html_page(cliopatria(default),
			title('Predicates in graph ~w'-[Graph]),
			[ h4(['Predicates in graph ', \graph_link(Graph)]),
			  \predicate_table(Sorted, Graph, [])
			]).

predicate_table(Preds, Graph, Options) -->
	{ option(top_max(TopMax), Options, 500),
	  option(bottom_max(BottomMax), Options, 500)
	},
	html(table(class(rdf_browse),
		   [ \predicate_table_header
		   | \table_rows_top_bottom(predicate_row(Graph), Preds,
					    TopMax, BottomMax)
		   ])).

predicate_table_header -->
	html(tr([ th('Predicate'),
		  th('#Triples'),
		  th('#Distinct subjects'),
		  th('#Distinct objects'),
		  th('Domain(s)'),
		  th('Range(s)')
		])).

%%	predicate_row(?Graph, +Pred) is det.

predicate_row(Graph, Pred) -->
	{ predicate_statistics(Graph, Pred, Triples,
			       Subjects, Objects, Doms, Ranges),
	  (   var(Graph)
	  ->  Params = [predicate(Pred)]
	  ;   Params = [graph(Graph), predicate(Pred)]
	  ),
	  http_link_to_id(list_triples,   Params, PLink)
	},
	html([ td(\resource_link(Pred)),
	       td(class(int), a(href(PLink), Triples)),
	       \resources(Subjects, subject, Params, []),
	       \resources(Objects, object, Params, []),
	       \resources(Doms, domain, Params, [force(true)]),
	       \resources(Ranges, range, Params, [force(true)])
	     ]).

resources([], _, _, _) --> !,
	html(td(class(empty), -)).
resources([One], _, _, Options) --> !,
	html(td(\resource_link(One, Options))).
resources(Many, What, Params, _) --> !,
	{ length(Many, Count),
	  http_link_to_id(list_predicate_resources, [side(What)|Params], Link)
	},
	html(td(class(cint), a(href(Link), Count))).


predicate_statistics(Graph, P, C, Subjects, Objects, Domains, Ranges) :-
	findall(S-O, rdf(S,P,O,Graph), Pairs),
	length(Pairs, C),
	pairs_keys_values(Pairs, Ss, Os),
	sort(Ss, Subjects),
	sort(Os, Objects),
	resources_types(Subjects, Graph, Domains),
	resources_types(Objects, Graph, Ranges).

resources_types(URIs, Graph, Types) :-
	findall(T, resource_type_in(URIs, Graph, T), TList),
	sort(TList, Types).

resource_type_in(List, Graph, T) :-
	member(URI, List),
	resource_type(URI, Graph, T).

%%	resource_type(+URI, +Graph, -Type) is det.

resource_type(URI, Graph, T) :-
	(   URI = literal(Lit)
	->  (   Lit = type(T, _)
	    ->	true
	    ;	rdf_equal(T, rdfs:'Literal')
	    )
	;   rdf(URI, rdf:type, T, Graph)
	*-> true
	;   rdf_equal(T, rdfs:'Resource')
	).


		 /*******************************
		 *	  LIST RESOURCES	*
		 *******************************/

%%	list_predicate_resources(+Request)
%
%	List sets of resources according to various specifications

list_predicate_resources(Request) :-
	http_parameters(Request,
			[ graph(Graph, [ optional(true) ]),
			  predicate(Pred, []),
			  side(Which, [oneof([subject,object,domain,range])]),
			  sortBy(Sort, [ oneof([label,frequency]),
					 default(frequency)
				       ]),
			  skosmap(SkosMap, [boolean, default(_)])
			]),
	do_skos(SkosMap, Which, Pred),
	findall(R, predicate_resource(Graph, Pred, Which, R), Set),
	term_frequency_list(Set, FPairs),
	sort_pairs_by_label(FPairs, TableByName),
	(   Sort == frequency
	->  reverse(TableByName, RevTableByName),
	    transpose_pairs(RevTableByName, FPairsUp),
	    reverse(FPairsUp, FPairsDown),
	    flip_pairs(FPairsDown, Table)
	;   Table = TableByName
	),

	pred_resource_options(Pred, Which, Options),

	reply_html_page(cliopatria(default),
			title(\resource_table_title(Graph, Pred, Which, Sort)),
			[ h4(\html_resource_table_title(Graph, Pred, Which,
							Sort, SkosMap)),
			  \resource_frequency_table(Table,
						    [ skosmap(SkosMap),
						      predicate(Pred),
						      side(Which),
						      sort(Sort)
						    | Options
						    ])
			]).

pred_resource_options(_, domain, [label('Class'), force(true)]) :- !.
pred_resource_options(_, range, [label('Class'), force(true)]) :- !.
pred_resource_options(_, _, []).

do_skos(SkosMap, _, _) :-
	nonvar(SkosMap), !.
do_skos(SkosMap, object, Pred) :-
	\+ rdf(_, Pred, literal(_)), !,
	SkosMap = false.
do_skos(SkosMap, object, _) :-
	rdfs_individual_of(_, skos:'ConceptScheme'), !,
	SkosMap = true.
do_skos(false, _, _).


resource_table_title(Graph, Pred, Which, Sort) -->
	{ label_of(Pred, PLabel)
	},
	html('Distinct ~ws for ~w in ~w sorted by ~w'-
	     [Which, PLabel, Graph, Sort]
	     ).

html_resource_table_title(Graph, Pred, Which, Sort, SkosMap) -->
	html([ 'Distinct ~ws'-[Which],
	       \for_predicate(Pred),
	       \in_graph(Graph),
	       \sorted_by(Sort),
	       \showing_skosmap(SkosMap)
	     ]).

for_predicate(Pred) -->
	{ var(Pred) }, !.
for_predicate(Pred) -->
	html([' for predicate ', \resource_link(Pred, [force(true)])]).

showing_skosmap(true) --> !,
	html(' with mapping to SKOS').
showing_skosmap(_) --> [].

resource_frequency_table(Pairs, Options) -->
	{ option(top_max(TopMax), Options, 500),
	  option(top_max(BottomMax), Options, 500),
	  option(predicate(Pred), Options, _),
	  option(side(Side), Options)
	},
	html(table(class(rdf_browse),
		   [ \resource_table_header(Options)
		   | \table_rows_top_bottom(resource_row(Pred, Side, Options), Pairs,
					    TopMax, BottomMax)
		   ])).

resource_table_header(Options) -->
	{ option(label(Label), Options, 'Resource'),
	  (   option(sort(Sort), Options)
	  ->  (   Sort == frequency
	      ->  A1 = [],
		  A2 = [class(sorted)]
	      ;	  A1 = [class(sorted)],
		  A2 = []
	      )
	  ;   A1 = [],
	      A2 = []
	  )
	},
	html(tr([ th(A1, Label),
		  th(A2, 'Count'),
		  \skosmap_head(Options)
		])).

skosmap_head(Options) -->
	{ option(skosmap(true), Options) }, !,
	html(th('SKOS mapping')).
skosmap_head(_) --> [].

resource_row(Pred, object, Options, R-C) --> !,
	{ object_param(R, Param),
	  http_link_to_id(list_triples_with_object,
	       [ p(Pred),
		 Param
	       ], HREF)
	},
	html([ td(\resource_link(R, Options)),
	       td(class(int), a(href(HREF), C)),
	       \skosmap(R, Options)
	     ]).
resource_row(Pred, Side, Options, R-C) -->
	{ domain_range_parameter(Side, R, Param), !,
	  http_link_to_id(list_triples,
	       [ predicate(Pred),
		 Param
	       ], HREF)
	},
	html([ td(\resource_link(R, Options)),
	       td(class(int), a(href(HREF), C)),
	       \skosmap(R, Options)
	     ]).
resource_row(_, _, Options, R-C) -->
	html([ td(\resource_link(R, Options)),
	       td(class(int), C),
	       \skosmap(R, Options)
	     ]).

object_param(R, r=R) :-
	atom(R), !.
object_param(L, l=A) :-
	term_to_atom(L, A).

domain_range_parameter(domain, R, domain(R)).
domain_range_parameter(range,  R, range(R)).

%%	skosmap(+Literal, +Options)//

skosmap(Literal, Options) -->
	{ Literal = literal(_),
	  option(skosmap(true), Options),
	  findall(Concept-Scheme, skos_find(Literal, Concept, Scheme), Pairs),
	  Pairs \== [],
	  sort_pairs_by_label(Pairs, Sorted)
	},
	html(td(\skos_references(Sorted))).
skosmap(_, _) --> [].

skos_find(Literal, Concept, Scheme) :-
	rdf_has(Concept, skos:prefLabel, Literal),
	rdf_has(Concept, skos:inScheme, Scheme).

skos_references([]) --> [].
skos_references([H|T]) -->
	skos_reference(H),
	(   { T == [] }
	->  []
	;   html('; '),
	    skos_references(T)
	).

skos_reference(Concept-Scheme) -->
	html([\resource_link(Concept), ' in ', \resource_link(Scheme)]).


%%	resource_link(+URI) is det.
%
%	Make a link to an arbitrary resource

resource_link(R) -->
	resource_link(R, []).

resource_link(R, Options) -->
	{ (   rdf(R, _, _)
	  ->  true
	  ;   option(force(true), Options)
	  ), !,
	  http_link_to_id(list_resource, [r=R], HREF)
	},
	html(a(href(HREF), \resource_label(R, Options))).
resource_link(R, Options) -->
	{ atom(R) }, !,
	html(span(class(undef), \resource_label(R, Options))).
resource_link(Literal, Options) -->
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
	       \literal_label(Literal))).
resource_link(Literal, _) -->
	literal_label(Literal).

resource_label(R, Options) -->
	{ option(resource_format(Format), Options) }, !,
	resource_flabel(Format, R).
resource_label(R, _) -->
	label(R).

resource_flabel(plain, R) --> !,
	html(R).
resource_flabel(nslabel, R) --> !,
	(   { rdf_global_id(NS:_Local, R), !,
	      label_property(P),
	      rdf_has(R, P, Value),
	      text_of_literal(Value, Label)
	    }
	->  html([span(class(ns),NS),':',span(class(rlabel),Label)])
	;   label(R)
	).

resource_flabel(_, R) -->
	label(R).

flip_pairs([], []).
flip_pairs([Key-Val|Pairs], [Val-Key|Flipped]) :-
	flip_pairs(Pairs, Flipped).

predicate_resource(Graph, Pred, subject, R) :- !,
	rdf(R, Pred, _, Graph).
predicate_resource(Graph, Pred, object, R) :-
	rdf(_, Pred, R, Graph).
predicate_resource(Graph, Pred, domain, D) :- !,
	rdf(R, Pred, _, Graph),
	rdf(R, rdf:type, D, Graph).
predicate_resource(Graph, Pred, range, R) :-
	rdf(_, Pred, O, Graph),
	resource_type(O, Graph, R).

%%	term_frequency_list(+Terms, -TermFrequencyPairs)
%
%	TermFrequencyPairs is a list if  pairs Value-Count of equivalent
%	term in Terms.  Equivalence is determined using ==/2.  The terms
%	themselves are sorted on the standard order of terms.

term_frequency_list(Resources, Pairs) :-
	msort(Resources, Sorted),
	fpairs(Sorted, Pairs).

fpairs([], []).
fpairs([H|T0], [H-C|T]) :-
	pick_same(T0, T1, H, 1, C),
	fpairs(T1, T).

pick_same([H1|T0], L, H, F0, F) :-
	H == H1, !,
	F1 is F0 + 1,
	pick_same(T0, L, H, F1, F).
pick_same(L, L, _, F, F).


		 /*******************************
		 *    LIST A SINGLE RESOURCE	*
		 *******************************/

%%	list_resource(+Request)
%
%	List the property table for a single resource (=local view)

list_resource(Request) :-
	http_parameters(Request,
			[ r(URI, []),
			  sorted(Sorted, [ oneof([default,none]),
					   default(default)
					 ]),
			  graph(Graph, [optional(true)])
			]),
	label_of(URI, Label),
	reply_html_page(cliopatria(default),
			title('Resource ~w'-[Label]),
			[ h4([ 'Local view for ',
			       \location(URI, Graph)
			     ]),
			  \local_view(URI, Graph, [sorted(Sorted)]),
			  p(\as_object(URI, Graph)),
			  \uri_info(URI, Graph)
			]).

%%	location(+URI, ?Graph) is det.
%
%	Show the URI. If the URI is a blank node, show its context using
%	Turtle notation.

location(URI, _Graph) -->
	{ rdf_is_bnode(URI), !,
	  findall(Path, path_to_non_bnode(URI, Path), Paths),
	  sort_by_length(Paths, PathsByLen),
	  partition(starts_bnode, PathsByLen, StartsBNode, StartsReal),
	  (   StartsReal = [Path|_]
	  ->  true
	  ;   last(StartsBNode, Path)
	  )
	},
	bnode_location(Path).
location(URI, _) -->
	html(URI).

bnode_location([P-URI]) --> !,
	html([ '[', \resource_link(P, [force(true)]), ' ',
	            \resource_link(URI),
	       ']'
	     ]).
bnode_location([P-URI|More]) --> !,
	html([ '[', div(class(bnode_attr),
			[ div(\resource_link(P, [force(true)])),
			  div(\resource_link(URI))
			]), ' ',
	       \bnode_location(More),
	       ']'
	     ]).
bnode_location([URI|More]) --> !,
	resource_link(URI),
	html(' '),
	bnode_location(More).
bnode_location([]) -->
	[].

path_to_non_bnode(URI, Path) :-
	path_to_non_bnode_rev(URI, [URI], RevPath),
	reverse(RevPath, Path).

path_to_non_bnode_rev(URI, Seen, [P-URI|Path]) :-
	(   rdf_is_bnode(URI),
	    rdf(S, P, URI),
	    \+ memberchk(S, Seen)
	*-> path_to_non_bnode_rev(S, [S|Seen], Path)
	;   fail
	).
path_to_non_bnode_rev(URI, _, [URI]).

starts_bnode([URI|_]) :-
	rdf_is_bnode(URI).

sort_by_length(ListOfLists, ByLen) :-
	map_list_to_pairs(length, ListOfLists, Pairs),
	keysort(Pairs, Sorted),
	pairs_values(Sorted, ByLen).

%%	as_object(+URI, +Graph) is det.
%
%	Show the places where URI is used as an object.

as_object(URI, Graph) -->
	{ findall(S-P, rdf(S,P,URI,Graph), Pairs),
	  sort(Pairs, Unique)
	},
	as_object_locations(Unique, URI, Graph).

as_object_locations([], _URI, _) --> !,
	html([ 'The resource does not appear as an object' ]).
as_object_locations([S-P], URI, _) --> !,
	html([ 'The resource appears as object in one triple:',
	       div(class(triple),
		   [ '{ ',
		     \resource_link(S), ', ',
		     \resource_link(P, [force(true)]), ', ',
		     \resource_link(URI)
		   , ' }'
		   ])
	     ]).
as_object_locations(List, URI, Graph) --> !,
	{ length(List, Len),
	  (   var(Graph)
	  ->  Extra = []
	  ;   Extra = [graph=Graph]
	  ),
	  http_link_to_id(list_triples_with_object, [r=URI|Extra], Link)
	},
	html([ 'The resource appears as object in ',
	       a(href(Link), [Len, ' triples'])
	     ]).

%%	local_view(+URI, +Graph, +Options) is det.

local_view(URI, Graph, Options) -->
	{ option(top_max(TopMax), Options, 500),
	  option(top_max(BottomMax), Options, 500),
	  po_pairs(URI, Graph, Pairs, Options)
	},
	html(table(class(rdf_browse),
		   [ \lview_header(Options)
		   | \table_rows_top_bottom(lview_row, Pairs,
					    TopMax, BottomMax)
		   ])).

lview_header(Options) -->
	{ option(sorted(Sorted), Options, default),
	  alt_sorted(Sorted, Alt),
	  re_link([sorted(Alt)], HREF)
	},
	html(tr([ th('Predicate'),
		  th(['Value (sorted: ', a(href(HREF), Sorted), ')'])
		])).

alt_sorted(default, none).
alt_sorted(none, default).


lview_row(P-OList) -->
	html([ td(class(predicate), \resource_link(P, [force(true)])),
	       td(class(object), \object_list(OList))
	     ]).

object_list([]) --> [].
object_list([H|T]) -->
	html(div(class(obj), \resource_link(H))),
	object_list(T).


po_pairs(S, Graph, Pairs, Options) :-
	option(sorted(none), Options), !,
	findall(P-[O], rdf(S,P,O,Graph), Pairs).
po_pairs(S, Graph, Pairs, _Options) :-
	var(Graph), !,
	findall(P-OL,
		setof(O, rdf(S,P,O), OL),
		Pairs0),
	sort_po(Pairs0, Pairs).
po_pairs(S, Graph, Pairs, _Options) :-
	findall(P-OL,
		setof(O, rdf(S,P,O,Graph), OL),
		Pairs0),
	sort_po(Pairs0, Pairs).

%%	sort_po(+Pairs, -Sorted) is det.
%
%	Sort a list of P-ValueList. This is   used  to keep the dominant
%	rdf, rdfs, skos, etc. properties in a   fixed order at the start
%	of the table.

sort_po(Pairs, Sorted) :-
	map_list_to_pairs(po_key, Pairs, Keyed),
	keysort(Keyed, KeySorted),
	pairs_values(KeySorted, Sorted).

po_key(P-_, Key) :-
	order(P, Key), !.
po_key(P-_, Key) :-
	label_sort_key(P, Key).

term_expansion(order(NS:Local, K), order(P, K)) :-
	rdf_global_id(NS:Local, P).

order(P, 1000) :-
	label_property(P).
order(P, 1001) :-
	rdfs_subproperty_of(P, skos:altLabel).
order(rdf:type,		2000).
order(rdfs:subClassOf,  2001).
order(rdfs:domain,	2002).
order(rdfs:range,	2003).
order(rdfs:comment,	3000).
order(rdfs:isDefinedBy,	3001).


%%	uri_info(+URI, +Graph)// is det.
%
%	Display additional info and actions about   a URI in the context
%	of the given graph.

uri_info(URI, Graph) -->
	uri_class_info(URI, Graph),
	uri_predicate_info(URI, Graph),
	context_graph(URI).

uri_class_info(URI, Graph) -->
	{ rdf_current_predicate(URI)
	}, !,
	html(h4('Predicate statistics')),
	predicate_table([URI], Graph, []).
uri_class_info(_,_) --> [].

uri_predicate_info(URI, Graph) -->
	{ \+ \+ rdf(_, rdf:type, URI, Graph)
	}, !,
	html(h4('Class statistics')),
	class_table([URI], Graph, []).
uri_predicate_info(_, _) --> [].


%%	context_graph(+URI)// is det.
%
%	Show graph with the context of URI

context_graph(URI) -->
	html([ h4('Context graph'),
	       \canviz_graph(context_graph(URI),
			     [ wrap_url(resource_link),
			       graph_attributes([ rankdir('RL')
						]),
			       shape_hook(shape(URI))
			     ])
	     ]).

resource_link(URI, HREF) :-
	http_link_to_id(list_resource, [r(URI)], HREF).

shape(Start, Start, [style(filled), fillcolor('#00ff00')]).

%%	context_graph(+URI, -Triples) is det.
%
%	Triples is a graph  that  describes   the  environment  of  URI.
%	Currently, the environment is defined as:
%
%	    * skos:related properties (using 1 step)
%	    * Transitive properties (using upto 3 steps).

context_graph(URI, RDF) :-
	findall(T, context_triple(URI, T), RDF0),
	sort(RDF0, RDF1),
	minimise_graph(RDF1, RDF2),		% remove inverse/symmetric/...
	bagify_graph(RDF2, RDF3, Bags, []), 	% Create bags of similar resources
	append(RDF3, Bags, RDF).

:- rdf_meta
	transitive_context(r),
	context(r).

context_triple(URI, Triple) :-
	transitive_context(CP),
	parents(URI, CP, Triples, [URI], 3),
	member(Triple, Triples).
context_triple(URI, rdf(URI, P, O)) :-
	context(R),
	rdf_has(URI, R, O, P).
context_triple(URI, rdf(S, P, URI)) :-
	context(R),
	rdf_has(S, R, URI, P).

parents(URI, Up, [rdf(URI, P, Parent)|T], Visited, MaxD) :-
	succ(MaxD2, MaxD),
	rdf_has(URI, Up, Parent, P),
	\+ memberchk(Parent, Visited),
	parents(Parent, Up, T, [Parent|Visited], MaxD2).
parents(_, _, [], _, _).

transitive_context(rdfs:subClassOf).
transitive_context(rdfs:subPropertyOf).
transitive_context(skos:broader).
transitive_context(P) :-
	rdfs_individual_of(P, owl:'TransitiveProperty'),
	rdf_predicate_property(P, rdfs_subject_branch_factor(BF)),
	BF < 2.0.

context(skos:related).

%%	list_triples(+Request)
%
%	List triples from a given specification

list_triples(Request) :-
	http_parameters(Request,
			[ graph(Graph, [ optional(true),
					 description('Limit triples to this predicate')
				       ]),
			  domain(Dom,  [ optional(true),
					 description('Restrict to subjects of this class')
				       ]),
			  range(Range, [ optional(true),
					 description('Restrict to objects of this class')
				       ]),
			  predicate(Pred, [])
			]),
	(   atom(Dom)
	->  findall(S-O, rdf_in_domain(S,Pred,O,Dom,Graph), Pairs0)
	;   atom(Range)
	->  findall(S-O, rdf_in_range(S,Pred,O,Range,Graph), Pairs0)
	;   findall(S-O, rdf(S,Pred,O,Graph), Pairs0)
	),
	sort(Pairs0, Pairs),
	sort_pairs_by_label(Pairs, Sorted),
	length(Pairs, Count),
	label_of(Pred, PLabel),
	reply_html_page(cliopatria(default),
			title('Triples for ~w in graph ~w'-[PLabel, Graph]),
			[ h4(\triple_header(Count, Pred, Dom, Range, Graph)),
			  \triple_table(Sorted, Pred, [])
			]).

rdf_in_domain(S,P,O,Dom,Graph) :-
	rdf(S, P, O, Graph),
	rdf_has(S, rdf:type, Dom).

rdf_in_range(S,P,O,Lit,Graph) :-
	rdf_equal(rdfs:'Literal', Lit), !,
	O = literal(_),
	rdf(S, P, O, Graph).
rdf_in_range(S,P,O,Rng,Graph) :-
	rdf_equal(rdfs:'Resource', Rng), !,
	rdf(S, P, O, Graph),
	atom(O).
rdf_in_range(S,P,O,Rng,Graph) :-
	rdf(S, P, O, Graph),
	rdf_has(O, rdf:type, Rng).


triple_header(Count, Pred, Dom, Range, Graph) -->
	html([ 'Table for the ~D triples'-[Count],
	       \for_predicate(Pred),
	       \with_domain(Dom),
	       \with_range(Range),
	       \in_graph(Graph)
	     ]).

with_domain(Dom) -->
	{ var(Dom) }, !.
with_domain(Dom) -->
	html([' with domain ', \resource_link(Dom, [force(true)])]).

with_range(Range) -->
	{ var(Range) }, !.
with_range(Range) -->
	html([' with range ', \resource_link(Range, [force(true)])]).


triple_table(SOList, Pred, Options) -->
	{ option(top_max(TopMax), Options, 500),
	  option(top_max(BottomMax), Options, 500)
	},
	html(table(class(rdf_browse),
		   [ \so_header(Pred)
		   | \table_rows_top_bottom(so_row(Pred), SOList,
					    TopMax, BottomMax)
		   ])).

so_header(_) -->
	html(tr([ th('Subject'),
		  th('Object')
		])).

so_row(_P, S-O) -->
	html([ td(class(subject), \resource_link(S)),
	       td(class(object),  \resource_link(O))
	     ]).


%%	list_triples_with_object(+Request)

list_triples_with_object(Request) :-
	http_parameters(Request,
			[ r(RObject,   [optional(true)]),
			  l(LObject,   [optional(true)]),
			  p(P,         [optional(true)]),
			  graph(Graph, [optional(true)])
			]),
	target_object(RObject, LObject, Object),
	findall(S-P, rdf(S,P,Object,Graph), Pairs0),
	sort(Pairs0, Pairs),
	sort_pairs_by_label(Pairs, Sorted),
	length(Pairs, Count),
	label_of(Object, OLabel),
	reply_html_page(cliopatria(default),
			title('Triples with object ~w'-[OLabel]),
			[ h4(\otriple_header(Count, Object, P, Graph)),
			  \otriple_table(Sorted, Object, [])
			]).

target_object(RObject, _LObject, RObject) :-
	atom(RObject), !.
target_object(_, LObject, Object) :-
	atom(LObject), !,
	term_to_atom(Object, LObject).
target_object(_, _, _) :-
	throw(existence_error(http_parameter, r)).

otriple_header(Count, Object, Pred, Graph) -->
	html([ 'Table for the ~D triples'-[Count],
	       \with_object(Object),
	       \on_predicate(Pred),
	       \in_graph(Graph)
	     ]).

with_object(Obj) -->
	{ var(Obj)}, !.
with_object(Obj) -->
	html([' with object ', \resource_link(Obj)]).

on_predicate(P) -->
	{ var(P) }, !.
on_predicate(P) -->
	html([' on predicate ', \resource_link(P, [force(true)])]).


otriple_table(SPList, Object, Options) -->
	{ option(top_max(TopMax), Options, 500),
	  option(top_max(BottomMax), Options, 500)
	},
	html(table(class(rdf_browse),
		   [ \sp_header(Object)
		   | \table_rows_top_bottom(sp_row(Object), SPList,
					    TopMax, BottomMax)
		   ])).

sp_header(_) -->
	html(tr([ th('Subject'),
		  th('Predicate')
		])).

sp_row(_O, S-P) -->
	html([ td(class(subject),   \resource_link(S)),
	       td(class(predicate), \resource_link(P, [force(true)]))
	     ]).





		 /*******************************
		 *	      RDF UTIL		*
		 *******************************/

%%	sort_by_label(+URIs, -Sorted) is det.
%
%	Sort a list of URIs by their label using locale-based ordering.

sort_by_label(URIs, Sorted) :-
	map_list_to_pairs(label_sort_key, URIs, LabelPairs),
	keysort(LabelPairs, SortedPairs),
	pairs_values(SortedPairs, Sorted).

label_sort_key(URI, Key) :-
	label_of(URI, Label),
	collation_key(Label, Key).

%%	sort_pairs_by_label(+Pairs, -Sorted)
%
%	Sort a pair-list where the keys are resources by their label.

sort_pairs_by_label(Pairs, Sorted) :-
	map_list_to_pairs(key_label_sort_key, Pairs, LabelPairs),
	keysort(LabelPairs, SortedPairs),
	pairs_values(SortedPairs, Sorted).

key_label_sort_key(R-_, Key) :-
	label_sort_key(R, Key).

%%	label_of(+RDFvalue, -Label) is det.
%
%	Provide a resource for something in the RDF graph.
%
%	@tbd	Support SKOS, deal with type and language, etc.
%		Shouldn't this be a rule?

label_of(literal(Literal), Label) :- !,
	literal_label(Literal, Label).
label_of(URI, Label) :-
	rdfs_label(URI, Label).

literal_label(type(_, Value), Value) :- !.
literal_label(lang(_, Value), Value) :- !.
literal_label(Value, Value).

%%	label(+Value)// is det.
%
%	Produce an HTML friendly representation for Value.
%
%	@tbd	Probe Linked Open data?
%	@tbd	Deal with blank nodes

label(R) -->
	{ atom(R),
	  rdf_global_id(NS:Local, R), !
	},
	html(['<', span(class(ns), NS), ':', span(class(local), Local), '>']).
label(R) -->
	{ atom(R),
	  label_property(P),
	  rdf_has(R, P, Value),
	  text_of_literal(Value, Label), !
	},
	html(Label).
label(R) -->
	{ rdf_is_bnode(R) },
	bnode_label(R), !.
label(R) -->
	{ atom(R) }, !,
	html(R).
label(literal(Lit)) -->
	literal_label(Lit).

%%	bnode_label(+Resource)// is semidet.

bnode_label(R) -->
	{ rdf(R, rdf:value, Value),
	  (   Value = literal(_)
	  ;   \+ rdf_is_bnode(Value)
	  )
	}, !,
	html(['[', \label(Value), '...]']).

%%	literal_label(+Literal)// is det.
%
%	Emit a literal as a double-quoted string.
%
%	@tbd	Implement possibility for a summary.

literal_label(literal(Lit)) --> !,
	literal_label(Lit).
literal_label(type(Type, Value)) --> !,
	html(span(class(literal),
		  ['"', span(class(ltext), Value), '"', span(class(tqual), '^^'), \label(Type)])).
literal_label(lang(Lang, Value)) --> !,
	html(span(class(literal),
		  ['"', span(class(ltext), Value), '"',
		   span(class(lqual), '@'), span(class(lang), Lang)])).
literal_label(Value) -->
	html(span(class(literal),
		  ['"', span(class(ltext), Value), '"'])).

label_property(P) :- rdf_equal(P, rdfs:label).
label_property(P) :- rdf_equal(P, skos:prefLabel).
label_property(P) :- rdf_equal(P, skos:altLabel).
label_property(P) :- rdf_equal(P, dc:title).

text_of_literal(literal(L), Text) :- !,
	text_of_literal(L, Text).
text_of_literal(type(_, Text), Text) :- !.
text_of_literal(lang(_, Text), Text) :- !.
text_of_literal(Text, Text).


		 /*******************************
		 *	  CUSTOMIZATION		*
		 *******************************/

%%	label(+Id, -Label)

label(source,	       'Source URL').
label(triples,	       'Triple count').
label(subject_count(G),
      ['# ', a(href(Link), subjects)]) :-
	http_link_to_id(list_instances, [graph=G], Link).
label(bnode_count(G),
      ['# ', a(href(Link), 'bnode subjects')]) :-
	http_link_to_id(list_instances, [graph=G, type=bnode], Link).
label(predicate_count(G),
      ['# ', a(href(Link), predicates)]) :-
	http_link_to_id(list_predicates, [graph=G], Link).
label(type_count(G),
      ['# Referenced ', a(href(Link), classes)]) :-
	http_link_to_id(list_classes, [graph=G], Link).


%%	re_link(+NewParams, -HREF) is det.

re_link(NewParams, HREF) :-
	http_current_request(Request),
	memberchk(path(Path), Request),
	(   memberchk(search(Params), Request)
	->  true
	;   Params = []
	),
	merge_options(NewParams, Params, AllParams),
	uri_query_components(Search, AllParams),
	uri_data(path, Data, Path),
	uri_data(search, Data, Search),
	uri_components(HREF, Data).


		 /*******************************
		 *	      SEARCH		*
		 *******************************/

%%	search(+Request)
%
%	HTTP handler to search for triples   that contain a literal that
%	matches a query.
%
%	@tbd	Produce a sensible search language.

search(Request) :-
	http_parameters(Request,
			[ q(QueryText, [length>=1])
			]),
	literal_query(QueryText, Query),
	rdf_find_literals(Query, Literals),
	phrase(ltriples(Literals), Triples),
	reply_html_page(cliopatria(default),
			title('Search results for ~q'-[Query]),
			[ h4('Search results for token "~q"'-[Query]),
			  \rdf_table(Triples, [])
			]).

literal_query(QueryText, Query) :-
	tokenize_atom(QueryText, Tokens), !,
	once(phrase(query(Query), Tokens)).
literal_query(QueryText, case(QueryText)).

query(Query) -->
	simple_query(Q1),
	(   eos
	->  {Query = Q1}
	;   query(Q2),
	    {Query = and(Q1,Q2)}
	).

eos([],[]).

simple_query(Token) -->
	['"',Token,'"'], !.
simple_query(not(Token)) -->
	[-, Token].
simple_query(case(Token)) -->
	[Token].

ltriples([]) -->
	[].
ltriples([H|T]) -->
	findall(rdf(S,P,literal(L)), rdf(S,P,literal(exact(H), L))),
	ltriples(T).

%%	rdf_table(+Triples, +Options)// is det.
%
%	Emit a table of triples.
%
%	@param Triples is a list of rdf(S,P,O).

rdf_table(Triples, Options) -->
	{ option(top_max(TopMax), Options, 500),
	  option(top_max(BottomMax), Options, 500)
	},
	html(table(class(rdf_browse),
		   [ tr([ th('Subject'), th('Predicate'), th('Object') ])
		   | \table_rows_top_bottom(triple, Triples,
					    TopMax, BottomMax)
		   ])).

triple(rdf(S,P,O)) -->
	html([ td(class(subject),   \resource_link(S)),
	       td(class(predicate), \resource_link(P)),
	       td(class(object),    \resource_link(O))
	     ]).


%%	rdf_search_form//
%
%	Provide a search form to find labels in the database.

rdf_search_form -->
	html_requires(css('rdf_browse.css')),
	html(form([ id(search_form),
		    action(location_by_id(search))
		  ],
		  [ div([ \search_box([ name(q) ]),
			  input([ type(submit),
				  value('Search')
				])
			])
		  ])).

max_results_displayed(100).

search_box(Options) -->
	{ max_results_displayed(Max)
	},
	autocomplete(ac_find,
		     [ query_delay(0.5),
		       auto_highlight(false),
		       max_results_displayed(Max),
		       width('30ex')
		     | Options
		     ]).

%%	autocomplete(+HandlerID, +Options)// is det.
%
%	Insert a YUI autocomplete widget that obtains its alternatives
%	from HandlerID.  The following Options are supported:
%
%	    * width(+Width)
%	    Specify the width of the box.  Width must satisfy the CSS
%	    length syntax.
%
%	    * query_delay(+Seconds)
%	    Wait until no more keys are typed for Seconds before sending
%	    the query to the server.

autocomplete(Handler, Options) -->
	{ http_location_by_id(Handler, Path),
	  atom_concat(Handler, '_complete', CompleteID),
	  atom_concat(Handler, '_input', InputID),
	  atom_concat(Handler, '_container', ContainerID),
	  select_option(width(Width), Options, Options1, '25em'),
	  select_option(name(Name), Options1, Options2, predicate),
	  select_option(value(Value), Options2, Options3, '')
	},
	html([ \html_requires(yui('autocomplete/autocomplete.js')),
	       \html_requires(yui('autocomplete/assets/skins/sam/autocomplete.css')),
	       div(id(CompleteID),
		   [ input([ id(InputID),
			     name(Name),
			     value(Value),
			     type(text)
			   ]),
		     div(id(ContainerID), [])
		   ]),
	       style(type('text/css'),
		     [ '#', CompleteID, '\n',
		       '{ width:~w; padding-bottom:0em; display:inline-block; vertical-align:top}'-[Width]
		     ]),
	       \autocomplete_script(Path, InputID, ContainerID, Options3)
	     ]).

highlight -->
	html(script(type('text/javascript'),
\[
  'function highlighMatches(str, query, cls)\n',
  '{ var pat = new RegExp(query, "gi");
     var sa = str.split(pat);
     var ma = str.match(pat);
     var i;
     var out = sa[0];\n',

  '  if ( !ma )
     { return str;
     }\n',

  '  for(i=0; i<ma.length; )
     { out += "<span class=\'"+cls+"\'>"+ma[i++]+"</span>";
       out += sa[i];
     }\n',

  'return out;
   }\n'
 ])).

autocomplete_script(HandlerID, Input, Container, Options) -->
	{ http_absolute_location(HandlerID, Path, [])
	},
	highlight,
	html(script(type('text/javascript'), \[
'{ \n',
'  var oDS = new YAHOO.util.XHRDataSource("~w");\n'-[Path],
'  oDS.responseType = YAHOO.util.XHRDataSource.TYPE_JSON;\n',
'  oDS.responseSchema = { resultsList:"results",
			  fields:["label","count","href"]
			};\n',
'  oDS.maxCacheEntries = 5;\n',
'  var oAC = new YAHOO.widget.AutoComplete("~w", "~w", oDS);\n'-[Input, Container],
'  oAC.resultTypeList = false;\n',
'  oAC.formatResult = function(oResultData, sQuery, sResultMatch) {
     var sLabel = highlighMatches(oResultData.label, sQuery, "acmatch");
     if ( oResultData.count > 1 ) {
       sLabel += " <span class=\\"account\\">("+oResultData.count+")</span>";
     }
     return sLabel;
   };\n',
'  oAC.itemSelectEvent.subscribe(function(sType, aArgs) {
     var oData = aArgs[2];
     window.location.href = oData.href;
   });\n',
\ac_options(Options),
'}\n'
					     ])).
ac_options([]) -->
	[].
ac_options([H|T]) -->
	ac_option(H),
	ac_options(T).

ac_option(query_delay(Time)) --> !,
	html([ '  oAC.queryDelay = ~w;\n'-[Time] ]).
ac_option(auto_highlight(Bool)) --> !,
	html([ '  oAC.autoHighlight = ~w;\n'-[Bool] ]).
ac_option(max_results_displayed(Max)) -->
	html([ '  oAC.maxResultsDisplayed = ~w;\n'-[Max] ]).
ac_option(O) -->
	{ domain_error(yui_autocomplete_option, O) }.

%%	ac_find(+Request)
%
%	Perform autocompletion for literals and resources

ac_find(Request) :-
	max_results_displayed(DefMax),
	http_parameters(Request,
			[ query(Query, []),
			  maxResultsDisplayed(Max, [integer, default(DefMax)])
			]),
	autocompletions(Query, Max, Count, Completions),
	reply_json(json([ query = json([ count=Count
				       ]),
			  results = Completions
			])).

autocompletions(Query, Max, Count, Completions)  :-
	autocompletions(prefix(label), Query, Max, BNC, ByName),
	(   BNC > Max
	->  Completions = ByName,
	    Count = BNC
	;   TMax is Max-BNC,
	    autocompletions(prefix(other), Query, TMax, BTC, ByToken),
	    append(ByName, ByToken, Completions),
	    Count is BNC+BTC
	).

autocompletions(How, Query, Max, Count, Completions) :-
	ac_objects(How, Query, Completions0),
	length(Completions0, Count),
	first_n(Max, Completions0, Completions1),
	maplist(obj_result, Completions1, Completions).

obj_result(Text-Count,
	   json([ label=Text,
		  count=Count,
		  href=Href
		])) :-
	object_href(Text, Href).

object_href(Text, Link) :- !,
	term_to_atom(literal(Text), Atom),
	http_link_to_id(list_triples_with_object, [ l=Atom ], Link).

first_n(0, _, []) :- !.
first_n(_, [], []) :- !.
first_n(N, [H|T0], [H|T]) :-
	N2 is N - 1,
	first_n(N2, T0, T).


%%	ac_objects(+How, +Query, -Objects)
%
%	@param Objects is a list of Text-Count pairs

ac_objects(How, Query, Objects) :-
	findall(Pair, ac_object(How, Query, Pair), Pairs),
	keysort(Pairs, KSorted),
	group_pairs_by_key(KSorted, Grouped),
	maplist(hit_count, Grouped, Objects).

hit_count(Text-Resources, Text-Count) :-
	length(Resources, Count).	% duplicates?


%%	ac_object(+How, +Query, -Object)

ac_object(prefix(label), Query, Text-Resource) :-
	rdf(Resource, P, literal(prefix(Query), Literal)),
	(   label_property(LP),
	    rdfs_subproperty_of(P, LP)
	->  text_of_literal(Literal, Text)
	).
ac_object(prefix(other), Query, Text-Resource) :-
	rdf(Resource, P, literal(prefix(Query), Literal)),
	(   label_property(LP),
	    rdfs_subproperty_of(P, LP)
	->  fail
	;   text_of_literal(Literal, Text)
	).


		 /*******************************
		 *     HTML INFRASTRUCTURE	*
		 *******************************/

:- meta_predicate
	html_property_table(?, 0).

html_property_table(Template, Goal) -->
	{ findall(Template, Goal, Rows) },
	html(table(class(properties),
		   \table_rows(prow, Rows))).

prow(Row) -->
	{ Row =.. [_,H|Cells],
	  (   label(H, Label0)
	  ->  true
	  ;   functor(H, Label0, _)
	  ),
	  (   is_list(Label0)
	  ->  append(Label0, [:], Label)
	  ;   Label = [Label0, :]
	  )
	},
	html([ th(Label)
	     | \pcells(Cells)
	     ]).

pcells([]) --> [].
pcells([H|T]) -->
	pcell(H),
	pcells(T).

pcell(int(Value)) -->
	{ integer(Value) }, !,
	html(td(class(int), '~D'-[Value])).
pcell(H) -->
	{ compound(H),
	  H =.. [Class,Value], !
	},
	html(td(class(Class), Value)).
pcell(H) -->
	html(td(H)).


%%	table_rows(:Goal, +DataList)//
%
%	Emit a number of table rows (=tr=).   The content of each row is
%	created by calling call(Goal, Data)  as   a  DCG.  The rows have
%	alternating classes =even= and =odd=.  The first row is =odd=.

:- meta_predicate
	table_rows(3, +),
	table_rows_top_bottom(3, +, +, +).

table_rows(Goal, Rows) -->
	table_rows(Rows, Goal, 1, -1).

table_rows_top_bottom(Goal, Rows, inf, inf) --> !,
	table_rows(Rows, Goal, 1, -1).
table_rows_top_bottom(Goal, Rows, MaxTop, MaxBottom) -->
	{ length(Rows, Count) },
	(   { MaxTop+MaxBottom >= Count }
	->  table_rows(Rows, Goal, 1, -1)
	;   { Skip is Count-MaxBottom,
	      delete_list_prefix(Skip, Rows, BottomRows),
	      Skipped is Count-(MaxTop+MaxBottom)
	    },
	    table_rows(Rows, Goal, 1, MaxTop),
	    html(tr(class(skip),
		    [ th(colspan(10), 'Skipped ~D rows'-[Skipped])
		    ])),
	    table_rows(BottomRows, Goal, 1, -1)
	).

table_rows(_, _, _, 0) --> !, [].
table_rows([], _, _, _) --> [].
table_rows([H|T], Goal, N, Left) -->
	{ N2 is N + 1,
	  (   N mod 2 =:= 0
	  ->  Class = even
	  ;   Class = odd
	  ),
	  Left2 is Left - 1
	},
	html(tr(class(Class), \call(Goal, H))),
	table_rows(T, Goal, N2, Left2).

delete_list_prefix(0, List, List) :- !.
delete_list_prefix(_, [], []) :- !.
delete_list_prefix(N, [_|T], List) :-
	N2 is N - 1,
	delete_list_prefix(N2, T, List).
