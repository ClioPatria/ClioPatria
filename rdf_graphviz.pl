/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007-2010, University of Amsterdam,
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

:- module(rdf_graphviz,
	  [ gviz_write_rdf/3		% +Stream, +Triples, +Options
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(http/html_write)).
:- use_module(library(assoc)).
:- use_module(library(option)).
:- use_module(library(gensym)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_label)).


/** <module> Interface to graphviz for RDF graphs

Graphviz is a general purpose graph vizualization library. Its home-page
is  http://www.graphviz.org/  This  module  translates    an  RDF  graph
represented as a list of rdf(S,P,O) into a .dot file.

@author	Jan Wielemaker
*/

%%	gviz_write_rdf(+Stream, +Graph, +Options) is det.
%
%	Write the graph Triples to Stream in =dot= compatible format.
%	Options:
%
%	    * graph_attributes(+Attributes)
%	    Additional overall graphs attributes for dot.  Each
%	    attribute is of the format Name(Value) and written
%	    as Name="Value".  The term size(W,H) is accepted as
%	    well.
%
%	    * max_label_length(+Len)
%	    Truncate labels to Len characters.  Default is 25.
%	    Use =inf= to print the full label.
%
%	    * lang(+Lang)
%	    Lang is the language used for the labels.  See
%	    resource_label/4.
%
%	    * bags(Bags)
%	    How to handle bags.  Values are
%
%	        * graph
%	        Show as normal nodes (i.e. handles as normal RDF)
%
%	        * merge(Shape, Max)
%	        Put the members in a dot node using Shape.  Only
%	        place the first Max members and add a note stating
%	        '... showing N of M' to indicate the actual number
%
%	    * wrap_url(:Goal)
%	    If present, URLs of the graph are replaced with the
%	    result of call(Goal, URL0, URL)

:- module_transparent
	gviz_write_rdf/3.

gviz_write_rdf(Stream, Graph, Options0) :-
	meta_options(rdf_graphviz:is_meta, Options0, Options),
	format(Stream, 'digraph G~n{ ', []),
	option(graph_attributes(Attrs), Options, []),
	write_graph_attributes(Attrs, Stream),
	write_graph_attributes([fontname('Verdana:style=Regular')], Stream), % Doesn't apper to work?
	combine_bags(Graph, Triples, Bags, Options),
	gv_write_edges(Triples, Done, Stream, Options),
	assoc_to_list(Done, Nodes),
	gv_write_nodes(Nodes, Stream, [bags(Bags)|Options]),
	format(Stream, '~n}~n', []).

is_meta(wrap_url).
is_meta(shape_hook).

%%	write_graph_attributes(+List, +Out)
%
%	Write attributes for the graph as a whole

write_graph_attributes([], _).
write_graph_attributes([H|T], Out) :-
	write_graph_attribute(H, Out),
	write_graph_attributes(T, Out).

write_graph_attribute(size(W,H), Out) :- !,
	format(Out, '  size="~w,~w";~n', [W, H]).
write_graph_attribute(AttVal, Out) :-
	AttVal =.. [Name, Value],
	format(Out, '  ~w="~w";~n', [Name, Value]).


%%	combine_bags(+Graph, -Triples, -Bags, +Options) is det.
%
%	Seperate  the  bags  from  the   graph.  Triples  represent  the
%	remaining graph (in which a bag is a single node) and Bags is an
%	assoc BagID-Members storing the members of the bags.

combine_bags(Graph, Graph, Bags, Options) :-
	option(bags(graph), Options), !,
	empty_assoc(Bags).
combine_bags(Graph, Triples, Bags, _Options) :-
	empty_assoc(Bags0),
	find_bags(Graph, Graph1, Bags0, Bags1),
	collect_bags(Graph1, Triples, Bags1, Bags).

find_bags([], [], Bags, Bags).
find_bags([rdf(S,P,O)|Graph], Triples, Bags0, Bags) :-
	rdf_equal(P, rdf:type),
	rdf_equal(O, rdf:'Bag'), !,
	put_assoc(S, Bags0, [], Bags1),
	find_bags(Graph, Triples, Bags1, Bags).
find_bags([H|T0], [H|T], Bags0, Bags) :-
	find_bags(T0, T, Bags0, Bags).

collect_bags([], [], Bags, Bags).
collect_bags([rdf(S,P,O)|Graph], Triples, Bags0, Bags) :-
	bagid_property(P),
	get_assoc(S, Bags0, L, Bags1, [O|L]), !,
	collect_bags(Graph, Triples, Bags1, Bags).
collect_bags([H|T0], [H|T], Bags0, Bags) :-
	collect_bags(T0, T, Bags0, Bags).


%	bagid_property(+P) is semidet.
%
%	True if P is of the format   =|_:N|=,  where N is a non-negative
%	integer.

bagid_property(P) :-
	atom_concat('_:', N, P),
	catch(atom_number(N, I), _, fail),
	integer(I), I >= 0.


%%	gv_write_edges(+Graph, -Done, +Stream, +Options) is det.
%
%	Write the edges of an RDF graph   in  =dot= format. It invents a
%	dot identifier for each node  as   it  processes  the nodes. The
%	mapping from node to dot  identifier   is  returned in the assoc
%	Done.

gv_write_edges(Graph, Done, Stream, Options) :-
	empty_assoc(Done0),
	gv_write_edges(Graph, Done0, Done, Stream, Options).

gv_write_edges([], Done, Done, _, _).
gv_write_edges([Triple|T], Done0, Done, Stream, Options) :-
	write_edge(Triple, Done0, Done1, Stream, Options),
	gv_write_edges(T, Done1, Done, Stream, Options).

write_edge(rdf(S,P,O), Done0, Done2, Stream, Options) :-
	format(Stream, '  ', []),
	write_node_id(S, Done0, Done1, Stream),
	write(Stream, ' -> '),
	write_node_id(O, Done1, Done2, Stream),
	resource_label(P, PL, Options),
	c_escape(PL, String),
	wrap_url(P, URL, Options),
	format(Stream, ' [url="~w" label="~s"];\n', [URL, String]).

write_node_id(S, Done, Done, Stream) :-
	get_assoc(S, Done, Id), !,
	write(Stream, Id).
write_node_id(S, Done0, Done, Stream) :-
	gensym(n, Id),
	put_assoc(S, Done0, Id, Done),
	write(Stream, Id).

%%	gv_write_nodes(+Nodes:list(pair), +Stream, +Options)
%
%	Write information about the nodes, defining  the share and label
%	of the node.

gv_write_nodes([], _, _).
gv_write_nodes([RDF-ID|T], Stream, Options) :-
	format(Stream, '~w ', [ID]),
	write_node_attributes(RDF, Stream, Options),
	write(Stream, ';\n  '),
	gv_write_nodes(T, Stream, Options).

%%	write_node_attributes(+RDF, +Stream, +Options) is det.
%
%	Write attributes for an RDF node.   The node identifier matching
%	the declared edges is alreadu written to Stream.

write_node_attributes(R, Stream, Options) :-
	rdf_is_resource(R),
	option(bags(Bags), Options),
	get_assoc(R, Bags, Members), !,
	Members = [First|_],
	shape(First, MemberShape, Options),
	option(bags(merge(BagShape, Max)), Options, merge([shape(box)], 5)),
	merge_options(BagShape, MemberShape, Shape),
	bag_label(Members, Max, Label, Options),
	write_attributes([html(Label)|Shape], Stream).
write_node_attributes(R, Stream, Options) :-
	rdf_is_resource(R), !,
	shape(R, Shape, Options),
	resource_label(R, Label, Options),
	wrap_url(R, URL, Options),
	write_attributes([url(URL), label(Label)|Shape], Stream).
write_node_attributes(Lit, Stream, Options) :-
	shape(Lit, Shape, Options),
	option(max_label_length(MaxLen), Options, 25),
	literal_text(Lit, Text),
	truncate_atom(Text, MaxLen, Summary),
	write_attributes([label(Summary)|Shape], Stream).

bag_label(Members, Max, Label, Options) :-
	length(Members, Len),
	phrase(html(table(border(0), \html_bag_label(Members, 1, Max, Len, Options))), Tokens),
	with_output_to(string(Label), print_html(Tokens)).

html_bag_label([], _, _, _, _) --> !.
html_bag_label(_, I, Max, Len, _Options) -->
	{ I > Max }, !,
	html(tr(td([align(right)],
		   font(face('Verdana:style=Italic'), '... showing ~D of ~D'-[Max, Len])))).
html_bag_label([H|T], I, Max, Len, Options) -->
	{ (   atom(H)
	  ->  wrap_url(H, URL, Options),
	      Atts=[href(URL)]
	  ;   Atts=[]
	  )
	},
	html(tr(td([align(left)|Atts], \html_resource_label(H, Options)))),
	{ I2 is I + 1 },
	html_bag_label(T, I2, Max, Len, Options).

html_resource_label(Resource, Options) -->
	{ resource_label(Resource, Label, Options)
	},
	html(Label).


%%	resource_label(+Resource, -Label:Atom, +Options) is det.
%
%	Label is the textual label to show for Resource. Process the
%	options
%
%	    * lang(+Lang)
%	    * max_label_length(+Len)

resource_label(Resource, Label, Options) :-
	(   option(lang(Lang), Options),
	    Literal = literal(lang(Lang)),
	    rdf_label(Resource, Literal)
	->  true
	;   rdf_label(Resource, Literal)
	->  true
	), !,
	literal_text(Literal, Text),
	option(max_label_length(MaxLen), Options, 25),
	truncate_atom(Text, MaxLen, Label).
resource_label(BNode, Label, Options) :-
	rdf_is_bnode(BNode), !,
	rdf_has(BNode, rdf:value, Value),
	resource_label(Value, ValueLabel, Options),
	format(atom(Label), '[~w ...]', [ValueLabel]).
resource_label(Resource, Label, _Options) :-
	uri_components(Resource, Components),
	(   uri_data(fragment, Components, Fragment),
	    nonvar(Fragment)
	->  Label = Fragment
	;   uri_data(path, Components, Path),
	    file_base_name(Path, Label)
	).


%%	write_attributes(+Attributes:list, +Out:stream) is det.
%
%	Write attribute values.  We define some special attributes:
%
%		* html(HTML)
%		Emit as label=<HTML>

write_attributes([], Out) :- !,
	format(Out, ' []').
write_attributes(List, Out) :- !,
	format(Out, ' [', []),
	write_attributes_2(List, Out),
	format(Out, ']', []).

write_attributes_2([], _).
write_attributes_2([H|T], Out) :-
	(   string_attribute(H)
	->  H =.. [Att, Value],
	    c_escape(Value, String),
	    format(Out, ' ~w="~s"', [Att, String])
	;   html_attribute(H, Att)
	->  arg(1, H, Value),
	    format(Out, ' ~w=<~s>', [Att, Value])
	;   H =.. [Name, Value],
	    format(Out, ' ~w=~w', [Name, Value])
	),
	write_attributes_2(T, Out).


string_attribute(label(_)).
string_attribute(url(_)).
string_attribute(fillcolor(_)).

html_attribute(html(_), label).


c_escape(Atom, String) :-
	atom_codes(Atom, Codes),
	phrase(cstring(Codes), String).

%%	cstring(+Codes)//
%
%	Create a C-string. Normally =dot=  appears   to  be  using UTF-8
%	encoding. Would there be a  safer   way  to  transport non-ascii
%	characters, such as \uXXXX?

cstring([]) -->
	[].
cstring([H|T]) -->
	(   cchar(H)
	->  []
	;   [H]
	),
	cstring(T).

cchar(0'") --> "\\\"".
cchar(0'\n) --> "\\n".
cchar(0'\t) --> "\\t".
cchar(0'\b) --> "\\b".

wrap_url(URL0, URL, Options) :-
	option(wrap_url(Wrap), Options),
	call(Wrap, URL0, URL), !.
wrap_url(URL, URL, _).


%%	shape(+Term, -Attributes, +Options) is det.
%
%	Shape is the shape of the node to use for Resource.  Processes the
%	option =shape_hook= using call(ShapeHook, Obj, Shape).

shape(Obj, Shape, Options) :-
	option(shape_hook(Hook), Options),
	call(Hook, Obj, Shape), !.
shape(Literal, [shape(box)], _) :-
	rdf_is_literal(Literal), !.
shape(_, [], _).
