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

:- module(rdf_abstract,
	  [ merge_sameas_graph/3,	% +GraphIn, -GraphOut, +Options
	    bagify_graph/4,		% +GraphIn, -GraphOut, -Bags, +Options
	    abstract_graph/3,		% +GraphIn, -GraphOut, +Options
	    minimise_graph/2,		% +GraphIn, -GraphOut

	    graph_resources/2,		% +Graph, -Resources
	    graph_resources/4		% +Graph, -Resources, -Predicates, -Types
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(assoc)).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(ordsets)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(settings)).

/** <module> Abstract RDF graphs

The task of this module is to do some simple manipulations on RDF graphs
represented as lists of rdf(S,P,O).  Supported operations:

	* merge_sameas_graph(+GraphIn, -GraphOut, +Options)
	Merge nodes by owl:sameAs

	* bagify_graph(+GraphIn, -GraphOut, -Bags, +Options)
	Bagify a graph, returning a new graph holding bags of resources
	playing a similar role in the graph.

	* abstract_graph(+GraphIn, -GraphOut, +Options)
	Abstract nodes or edges using rdf:type, rdfs:subClassOf and/or
	rdfs:subPropertyOf
*/

%%	merge_sameas_graph(GraphIn, GraphOut, +Options) is det.
%
%	Collapse nodes in GraphIn that are   related through an identity
%	mapping.  By  default,  owl:sameAs  is  the  identity  relation.
%	Options defines:
%
%	    * predicate(-PredOrList)
%	    Use an alternate or list of predicates that are to be
%	    treated as identity relations.
%
%	    * sameas_mapped(-Assoc)
%	    Assoc from resources to the resource it was mapped to.

:- rdf_meta
	merge_sameas_graph(+, -, t).

merge_sameas_graph(GraphIn, GraphOut, Options) :-
	sameas_spec(Options, SameAs),
	sameas_map(GraphIn, SameAs, Assoc),		% R->EqSet
	(   empty_assoc(Assoc)
	->  GraphOut = GraphIn,
	    empty_assoc(EqMap)
	;   assoc_to_list(Assoc, List),
	    pairs_values(List, EqSets),
	    sort(EqSets, UniqueEqSets),
	    map_list_to_pairs(rdf_representative, UniqueEqSets, Keyed),	% Repr-EqSet
	    representer_map(Keyed, EqMap),
	    map_graph(GraphIn, EqMap, GraphOut),
	    (	debugging(abstract)
	    ->	length(GraphIn, Before),
		length(GraphOut, After),
		debug(abstract, 'owl:sameAs reduction: ~D --> ~D edges', [Before, After])
	    ;	true
	    )
	),
	option(sameas_mapped(EqMap), Options, _).

sameas_spec(Options, SameAs) :-
	rdf_equal(owl:sameAs, OwlSameAs),
	option(predicate(SameAs0), Options, OwlSameAs),
	(   is_list(SameAs0)
	->  SameAs = SameAs0
	;   SameAs = [SameAs0]
	).

%%	sameas_map(+Graph, +SameAs, -Map:assoc) is det.
%
%	Create an assoc with R->Set, where   Set contains an ordered set
%	of resources equivalent to R.

sameas_map(Graph, SameAs, Assoc) :-
	empty_assoc(Assoc0),
	sameas_map(Graph, SameAs, Assoc0, Assoc).

sameas_map([], _, Assoc, Assoc).
sameas_map([rdf(S, P, O)|T], SameAs, Assoc0, Assoc) :-
	same_as(P, SameAs),
	S \== O, !,
	(   get_assoc(S, Assoc0, SetS)
	->  (   get_assoc(O, Assoc0, SetO)
	    ->	ord_union(SetO, SetS, Set)
	    ;	ord_union([O], SetS, Set)
	    )
	;   (   get_assoc(O, Assoc0, SetO)
	    ->	ord_union([S], SetO, Set)
	    ;   sort([S,O], Set)
	    )
	),
	putall(Set, Assoc0, Set, Assoc1),
	sameas_map(T, SameAs, Assoc1, Assoc).
sameas_map([_|T], SameAs, Assoc0, Assoc) :-
	sameas_map(T, SameAs, Assoc0, Assoc).

putall([], Assoc, _, Assoc).
putall([H|T], Assoc0, Value, Assoc) :-
	put_assoc(H, Assoc0, Value, Assoc1),
	putall(T, Assoc1, Value, Assoc).


%%	same_as(+Predicate:resource, +SameAs:list) is semidet.
%
%	True if Predicate expresses a same-as mapping.

same_as(P, Super) :-
	member(S, Super),
	rdfs_subproperty_of(P, S), !.


%%	representer_map(+List:list(Repr-Set), -Assoc) is det.
%
%	Assoc maps all elements of Set to its representer.

representer_map(Keyed, EqMap) :-
	empty_assoc(Assoc0),
	representer_map(Keyed, Assoc0, EqMap).

representer_map([], Assoc, Assoc).
representer_map([R-Set|T], Assoc0, Assoc) :-
	putall(Set, Assoc0, R, Assoc1),
	representer_map(T, Assoc1, Assoc).


		 /*******************************
		 *	       BAGIFY		*
		 *******************************/

%%	bagify_graph(+GraphIn, -GraphOut, -Bags, +Options) is det.
%
%	If a graph contains multiple objects of the same type (class) in
%	the same location in the graph (i.e.   all  links are the same),
%	create a *bag*. The bag is   represented by a generated resource
%	of type rdf:Bag and the RDF for the   bags  is put in Bags. I.e.
%	appending GraphOut and Bags provides a proper RDF model. Options
%	provides additional abstraction properties.  In particular:
%
%	    * class(+Class)
%	    Try to bundle objects under Class rather than their
%	    rdf:type.  Multiple of these options may be defined
%
%	    * property(+Property)
%	    Consider predicates that are an rdfs:subPropertyOf
%	    Property the same relations.
%
%	    * bagify_literals(+Bool)
%	    If =true= (default), also try to put literals into a
%	    bag.  Works well to collapse non-preferred labels.
%
%	@tbd Handle the property option

:- rdf_meta
	bagify_graph(+, -, -, t).

bagify_graph(GraphIn, GraphOut, Bags, Options) :-
	canonise_options(Options, Options1),
	partition_options(class, Options1, ClassOptions, Options2),
	graph_node_edges(GraphIn, AssocNodesToEdges, Options2),
	assoc_to_list(AssocNodesToEdges, NodesToEdges),
	pairs_keys(NodesToEdges, Nodes),
	group_resources_by_class(Nodes, ByClass, ClassOptions),
	resource_bags(ByClass, NodesToEdges, RawBags),
	(   debugging(abstract)
	->  length(RawBags, Len),
	    maplist(length, RawBags, BagLens),
	    sumlist(BagLens, ObjCount),
	    debug(abstract, 'Created ~D bags holding ~D objects', [Len, ObjCount])
	;   true
	),
	assign_bagids(RawBags, IDBags),
	representer_map(IDBags, Assoc),
	map_graph(GraphIn, Assoc, GraphOut0),
	merge_properties(GraphOut0, GraphOut, Options2),
	make_rdf_graphs(IDBags, Bags).

partition_options(Name, Options, WithName, WithoutName) :-
	partition(option_name(Name), Options, WithName, WithoutName).

option_name(Name, Option) :-
	functor(Option, Name, 1).

%%	canonise_options(+OptionsIn, -OptionsOut) is det.
%
%	Rewrite option list from possible Name=Value to Name(Value)

canonise_options(In, Out) :-
	memberchk(_=_, In), !,		% speedup a bit if already ok.
	canonise_options2(In, Out).
canonise_options(Options, Options).

canonise_options2([], []).
canonise_options2([Name=Value|T0], [H|T]) :- !,
	H =.. [Name,Value],
	canonise_options2(T0, T).
canonise_options2([H|T0], [H|T]) :- !,
	canonise_options2(T0, T).




%%	group_resources_by_class(+Resources, -ByClass, +Options) is det.
%
%	ByClass is a list of lists of  resources that belong to the same
%	class. First step we process the classes specified in Options.

group_resources_by_class([], [], _) :- !.
group_resources_by_class(Resources, ByClass, Options) :-
	select_option(class(Class), Options, Options1), !,
	(   partition(has_class(sub_class, Class), Resources, InClass, NotInClass),
	    InClass \== []
	->  ByClass = [InClass|ByClass1],
	    group_resources_by_class(NotInClass, ByClass1, Options1)
	;   group_resources_by_class(Resources, ByClass, Options1)
	).
group_resources_by_class([H|T0], [[H|S]|T], Options) :-
	class_of(H, exact, Class),
	partition(has_class(exact, Class), T0, S, T1),
	group_resources_by_class(T1, T, Options).

%%	has_class(+Match, +Class, +Node) is semidet.

has_class(Match, Class, Node) :-
	class_of(Node, Match, Class).

%%	class_of(+Node, +Match, -Class) is det.
%%	class_of(+Node, +Match, +Class) is semidet.

class_of(Node, sub_class, Class) :- !,
	rdfs_individual_of(Node, Class), !.
class_of(literal(_), exact, Literal) :- !,
	rdf_equal(Literal, rdfs:'Literal').
class_of(R, exact, Class) :-
	rdf_has(R, rdf:type, Class), !.
class_of(_, exact, Class) :-
	rdf_equal(Class, rdfs:'Resource').


%%	resource_bags(+ByClass:list(list(resource)),
%%		      +NodeToEdges:list(node-list(edges)),
%%		      -RawBags:list(list(resource))) is det.
%
%	Find bags of resources that have the same connections.

resource_bags(ByClass, NodeToEdges, Bags) :-
	phrase(resource_bags(ByClass, NodeToEdges), Bags).

resource_bags([], _) -->
	[].
resource_bags([ByClassH|ByClassT], NodeToEdges) -->
	{ sort(ByClassH, SortedNodes),
	  ord_subkeys(SortedNodes, NodeToEdges, SubNodeToEdges),
	  same_edges(SubNodeToEdges, Bags)
	},
	Bags,
	resource_bags(ByClassT, NodeToEdges).

%%	ord_subkeys(+Keys, +Pairs, -SubPairs) is det.
%
%	SubPairs is the sublist of Pairs with a key in Keys.
%
%	@param Keys	Sorted list of keys
%	@param Pairs	Key-sorted pair-list
%	@param SubPairs	Key-sorted pair-list

ord_subkeys([], _, []).
ord_subkeys([K|KT], [P|PT], Pairs) :-
	P = PK-_,
	compare(Diff, K, PK),
	ord_subkeys(Diff, K, KT, P, PT, Pairs).

ord_subkeys(=, _, KT, P, PT, [P|Pairs]) :- !,
	ord_subkeys(KT, PT, Pairs).
ord_subkeys(<, _, [K|KT], P, PT, Pairs) :-
	P = PK-_,
	compare(Diff, K, PK),
	ord_subkeys(Diff, K, KT, P, PT, Pairs).
ord_subkeys(>, K, KT, _, [P|PT], Pairs) :-
	P = PK-_,
	compare(Diff, K, PK),
	ord_subkeys(Diff, K, KT, P, PT, Pairs).


%%	same_edges(+NodeToEdges:list(node-edges), -Bags:list(list)) is det.
%
%	Bags is a list of lists of resources (nodes) that share the same
%	(abstracted) edges with the rest of the graph.

same_edges(NodeToEdges, Bags) :-
	transpose_pairs(NodeToEdges, ByEdges),		% list(edges-node)
	keysort(ByEdges, Sorted),
	group_pairs_by_key(Sorted, Grouped),
	pairs_values(Grouped, AllBySameEdge),
	include(longer_than_one, AllBySameEdge, Bags).

longer_than_one([_,_|_]).

%%	graph_node_edges(+Graph, -NodeEdges:assoc, +Options) is det.
%
%	NodeEdges is an assoc from resource to a sorted list of involved
%	triples. Only subject and objects are considered.
%
%	Processes =bagify_literals= and =property= options

graph_node_edges(Graph, Assoc, Options) :-
	option(bagify_literals(LitToo), Options, true),
	property_map(Options, Map0),
	empty_assoc(Assoc0),
	graph_node_edges(Graph, LitToo, Map0, Assoc0, Assoc1),
	map_assoc(sort, Assoc1, Assoc).

graph_node_edges([], _, _, Assoc, Assoc).
graph_node_edges([rdf(S,P,O)|T], LitToo, Map, Assoc0, Assoc) :-
	abstract_property(P, Map, SP, Map1),
	add_assoc(S, Assoc0, rdf(-, SP, O), Assoc1),
	(   (atom(O) ; LitToo == true )
	->  add_assoc(O, Assoc1, rdf(S, SP, -), Assoc2)
	;   Assoc2 = Assoc1
	),
	graph_node_edges(T, LitToo, Map1, Assoc2, Assoc).

add_assoc(Key, Assoc0, Value, Assoc) :-
	get_assoc(Key, Assoc0, Old, Assoc, [Value|Old]), !.
add_assoc(Key, Assoc0, Value, Assoc) :-
	put_assoc(Key, Assoc0, [Value], Assoc).


%%	property_map(+Options, -Map:assoc(P-Super))
%
%	Process the options, creating a map that replaces a property by
%	its registered super.

property_map(Options, Map) :-
	empty_assoc(Map0),
	property_map(Options, Map0, Map).

property_map([], Map, Map).
property_map([property(P)|T], Map0, Map) :- !,
	(   rdfs_subproperty_of(P, Super),
	    get_assoc(Super, Map0, Root)
	->  put_assoc(P, Map0, Root, Map1)
	;   put_assoc(P, Map0, P, Map1)
	),
	property_map(T, Map1, Map).
property_map([_|T], Map0, Map) :-
	property_map(T, Map0, Map).

%%	abstract_property(+P0, +Map0, -P, -Map) is det.
%
%	Find the abstract property for some property P.

abstract_property(P0, Map0, P, Map) :-
	get_assoc(P0, Map0, P), !,
	Map = Map0.
abstract_property(P, Map0, Root, Map) :-
	rdfs_subproperty_of(P, Super),
	get_assoc(Super, Map0, Root), !,
	debug(abstract(property), 'Mapped ~p --> ~p', [P, Root]),
	put_assoc(P, Map0, Root, Map).
abstract_property(P, Map, P, Map).


%%	assign_bagids(+Bags:list(bag), -IDBags:list(id-bag)).
%
%	Assign bag identifiers to the each bag in Bags.

assign_bagids(Bags, IDBags) :-
	assign_bagids(Bags, 1, IDBags).

assign_bagids([], _, []).
assign_bagids([H|T0], I, [Id-H|T]) :-
	atom_concat('__bag_', I, Id),
	I2 is I + 1,
	assign_bagids(T0, I2, T).


%%	make_rdf_graphs(+IDBags, -RDFBags) is det.
%
%	Translate BagID-Members into an RDF graph.

:- rdf_meta
	statement(r,r,o,?,?).			% statement//3

make_rdf_graphs(IDBags, RDFBags) :-
	phrase(make_rdf_graphs(IDBags), RDFBags).

make_rdf_graphs([]) -->
	[].
make_rdf_graphs([ID-Members|T]) -->
	statement(ID, rdf:type, rdf:'Bag'),
	bag_members(Members, 0, ID),
	make_rdf_graphs(T).

bag_members([], _, _) -->
	[].
bag_members([H|T], I, ID) -->
	{ I2 is I + 1,
	  atom_concat('_:', I, P)
	},
	statement(ID, P, H),
	bag_members(T, I2, ID).

statement(S, P, O) -->
	[ rdf(S, P, O) ].



		 /*******************************
		 *	 MERGE PROPERTIES	*
		 *******************************/

%%	merge_properties(+GraphIn, -GraphOut, +Options) is det.
%
%	Merge equivalent properties joining the same nodes.  They are
%	replaced by their common ancestors.
%
%	@param GraphIn	List of rdf(S,P,O)
%	@param GraphOut List of rdf(S,P,O)
%	@param Options  Option list (unused)

merge_properties([], [], _).
merge_properties([rdf(S,P,O)|GraphIn], GraphOut, Options) :-
	memberchk(rdf(S,_,O), GraphIn), !,
	partition(same_so(S,O), GraphIn, Same, Rest),
	maplist(pred, Same, Preds),
	sort([P|Preds], UniquePreds),
	common_ancestor_forest(sub_property_of, UniquePreds, Forest),
	pairs_keys(Forest, Roots),
	debug(abstract, 'Merged ~p --> ~p', [UniquePreds, Roots]),
	mk_p_triples(Roots, S, O, GraphOut, Out2),
	merge_properties(Rest, Out2, Options).
merge_properties([Triple|GraphIn], [Triple|GraphOut], Options) :-
	merge_properties(GraphIn, GraphOut, Options).

same_so(S, O, rdf(S, _, O)).
pred(rdf(_,P,_), P).

mk_p_triples([], _, _) --> [].
mk_p_triples([P|T], S, O) -->
	[rdf(S,P,O)],
	mk_p_triples(T, S, O).

sub_property_of(P, Super) :-
	rdf_has(P, rdfs:subPropertyOf, Super).


%%	common_ancestor_forest(:Pred, +Objects, -Forest) is det.
%
%	Forest is a minimal set  of   minimal  spanning  trees with real
%	branching (more than one child per   node) covering all Objects.
%	The partial ordering is defined   by  the non-deterministic goal
%	call(Pred, +Node, -Parent).
%
%		* Build up a graph represented as Node->Children and
%		a list of roots.  The initial list of roots is Objects.
%		The graph is built using breath-first search to minimize
%		depth.
%
%		* Once we have all roots, we delete all branches that
%		have only a single child.
%
%	@param	Forest	is a list of trees.  Each tree is represented
%		as Root-Children, where Children is a possibly
%		empty list if sub-trees.
%
%	@tbd	First prune dead-ends?
%
%		==
%		rdf_db:rdf_global_term([ulan:assisted_by, ulan:cousin_of], In),
%		gtrace,
%		rdf_abstract:common_ancestor_forest(sub_property_of, In, Out).
%		==

:- meta_predicate
	common_ancestor_forest(2, +, -).

common_ancestor_forest(Pred, Objects, Forest) :-
	strip_module(Pred, M, P),
	sort(Objects, Objects1),
	keys_to_assoc(Objects1, target*[], Nodes0),
	ancestor_tree(Objects1, M:P, Nodes0, Nodes, Roots),
	prune_forest(Nodes, Roots, Forest),
	debug(common_ancestor, 'Ancestors of ~p: ~p', [Objects1, Forest]).

%%	keys_to_assoc(+Keys:list, +Value, -Assoc) is det.
%
%	True if Assoc is an assoc where each Key maps to Value.

keys_to_assoc(Keys, Value, Assoc) :-
	empty_assoc(Assoc0),
	keys_to_assoc(Keys, Assoc0, Value, Assoc).

keys_to_assoc([], Assoc, _, Assoc).
keys_to_assoc([H|T], Assoc0, Value, Assoc) :-
	put_assoc(H, Assoc0, Value, Assoc1),
	keys_to_assoc(T, Assoc1, Value, Assoc).


ancestor_tree(Objects, Pred, Nodes0, Nodes, Roots) :-
	ancestor_tree(Objects, [], Objects, Pred, Nodes0, Nodes, Roots).

%%	ancestor_tree(+Open, +Closed, +Targets, :Pred, +NodesIn,
%%		      -NodesOut, -Roots) is det.
%
%	Explore the ancestor graph one more step. This is the main loop
%	looking for a spanning tree.  We are done if
%
%		* There is only one open node left and no closed ones.
%		We found the single common root.
%
%		* No open nodes are left.  We have a set of closed roots
%		which form our starting points.  We still have to figure
%		out the minimal set of these, as some of the trees may
%		overlap others.
%
%		* We have an open node covering all targets. This is the
%		lowest one as we used breath-first expansion.  This step
%		is too expensive.

ancestor_tree([One], [], _, _, Nodes, Nodes, [One]) :- !.
ancestor_tree([], Closed, _, _, Nodes, Nodes, Closed) :- !.
ancestor_tree(Open, _, Objects, _, Nodes, Nodes, [One]) :-
	member(One, Open),
	tree_covers(One, Nodes, Objects), !.
ancestor_tree(Open, Closed, Objects, Pred, Nodes0, Nodes, Roots) :-
	expand_ancestor_tree(Open, NewOpen, NewClosed, Closed, Nodes0, Nodes1, Pred),
	ancestor_tree(NewOpen, NewClosed, Objects, Pred, Nodes1, Nodes, Roots).


%%	expand_ancestor_tree(+Open0, -Open,
%%			     +Closed0, -Closed,
%%			     +Nodes0, -Nodes,
%%			     :Pred)
%
%	Expand the explored graph with one level. Open are the currently
%	open nodes. Closed  are  the  nodes   that  have  no  parent and
%	therefore are roots.
%
%	@param Nodes	is an assoc R->(State*list(Child))

expand_ancestor_tree([], [], Closed, Closed, Nodes, Nodes, _).
expand_ancestor_tree([H|T], Open, Closed0, Closed, Nodes0, Nodes, Pred) :-
	setof(Parent, call(Pred, H, Parent), Parents), !,
	add_parents(Parents, H, Open, OpenT, Nodes0, Nodes1),
	expand_ancestor_tree(T, OpenT, Closed0, Closed, Nodes1, Nodes, Pred).
expand_ancestor_tree([H|T], Open, [H|ClosedT], Closed, Nodes0, Nodes, Pred) :-
	expand_ancestor_tree(T, Open, ClosedT, Closed, Nodes0, Nodes, Pred).


%%	add_parents(+Parents:list, +Child, -NR, +NRT, +Nodes0, -Nodes)
%
%	Add links Parent->Child to the tree  Nodes0. The difference list
%	NR\NRT contains Parents added new to the tree.

add_parents([], _, NP, NP, Nodes, Nodes).
add_parents([H|T], Child, NP, NPT, Nodes0, Nodes) :-
	in_tree(Child, H, Nodes0), !,
	add_parents(T, Child, NP, NPT, Nodes0, Nodes).
add_parents([H|T], Child, NP, NPT, Nodes0, Nodes) :-
	get_assoc(H,
		  Nodes0, State*Children,
		  Nodes1, State*[Child|Children]), !,
	add_parents(T, Child, NP, NPT, Nodes1, Nodes).
add_parents([H|T], Child, [H|NP], NPT, Nodes0, Nodes) :-
	put_assoc(H, Nodes0, node*[Child], Nodes1),
	add_parents(T, Child, NP, NPT, Nodes1, Nodes).


%%	in_tree(?Node, +Root, +Nodes) is nondet.
%
%	True if Node appears in the tree below Root.

in_tree(Node, Node, _).
in_tree(Node, Root, Nodes) :-
	get_assoc(Root, Nodes, _State*Children),
	member(Child, Children),
	in_tree(Node, Child, Nodes).


%%	prune_forest(+Nodes, +Roots, -MinimalForest) is det.
%
%	MinimalForest is the minimal forest overlapping all targets.
%
%	@tbd Currently doesn't remove unnecessary trees.

prune_forest(Nodes, Roots, Forest) :-
	maplist(prune_root(Nodes), Roots, Roots1),
	sort(Roots1, Roots2),
	maplist(prune_ancestor_tree(Nodes), Roots2, Forest0),
	sort(Forest0, Forest).

%%	prune_root(+Nodes, +Root0, -Root) is det.
%
%	Prune the parts of the search tree   that  ended up nowhere. The
%	first real branch is where  we  find   a  solution  or there are
%	multiple parents. This avoids  doing   double  work  pruning the
%	trees itself.

prune_root(Nodes, Root0, Root) :-
	get_assoc(Root0, Nodes, node*[One]), !,
	prune_root(Nodes, One, Root).
prune_root(_, Root, Root).

%%	prune_ancestor_tree(Nodes, Root, Tree) is det.
%
%	Tree is a pruned hierarchy from Root using the branching paths of
%	Nodes.

prune_ancestor_tree(Nodes, Root, Tree) :-
	get_assoc(Root, Nodes, Value),
	(   Value = node*[One]
	->  prune_ancestor_tree(Nodes, One, Tree)
	;   Tree = (Root-Children),
	    Value = _*Children0,
	    maplist(prune_ancestor_tree(Nodes), Children0, Children)
	).

%%	tree_covers(+Root, +Nodes, -Targets:list) is det.
%
%	True if Targets is the sorted  list   of  targets covered by the
%	tree for which Root is the root.

tree_covers(Root, Nodes, Targets) :-
	phrase(tree_covers(Root, Nodes), Targets0),
	sort(Targets0, Targets).

tree_covers(Root, Nodes) -->
	{ get_assoc(Root, Nodes, State*Children) },
	(   {State == target}
	->  [Root]
	;   []
	),
	tree_covers_list(Children, Nodes).

tree_covers_list([], _) -->
	[].
tree_covers_list([H|T], Nodes) -->
	tree_covers(H, Nodes),
	tree_covers_list(T, Nodes).


		 /*******************************
		 *	    PRIMITIVES		*
		 *******************************/

%%	map_graph(+GraphIn, +Map:assoc, -GraphOut) is det.
%
%	Map a graph to a new graph  by   mapping  all  fields of the RDF
%	statements over Map. Then delete   duplicates from the resulting
%	graph as well as rdf(S,P,S) links that did not appear before the
%	mapping.
%
%	@tbd	Should we look inside literals for mapped types?  That
%		would be consistent with abstract_graph/3.

map_graph(GraphIn, Map, GraphOut) :-
	phrase(map_triples(GraphIn, Map), Graph2),
	sort(Graph2, GraphOut).

map_triples([], _) -->
	[].
map_triples([H0|T0], Map) -->
	map_triple(H0, Map),
	map_triples(T0, Map).

map_triple(rdf(S0,P0,O0), Map) -->
	{ map_resource(S0, Map, S),
	  map_resource(P0, Map, P),
	  map_object(O0, Map, O)
	},
	(   { S == O, S0 \== O0 }
	->  []
	;   [ rdf(S,P,O) ]
	).

map_resource(N0, Map, N) :-
	get_assoc(N0, Map, N), !.
map_resource(N, _, N).

map_object(O0, Map, O) :-
	get_assoc(O0, Map, O), !.
map_object(literal(type(T0, V)), Map, L) :-
	get_assoc(T0, Map, T), !,
	L = literal(type(T, V)).
map_object(O, _, O).


%%	map_graph(+GraphIn, +Map:assoc, -GraphOut, -AbstractMap) is det.
%
%	Map a graph to a new graph  by   mapping  all  fields of the RDF
%	statements over Map. The nodes in these  graphs are terms of the
%	form Abstract-list(concrete).
%
%	@param AbstractMap assoc Abstract -> ordset(concrete)

map_graph(GraphIn, Map, GraphOut, AbstractMap) :-
	map_graph(GraphIn, Map, GraphOut),
	assoc_to_list(Map, ConcAbstr),	% Concrete->Abstract
	graph_nodes(GraphIn, AllConcrete),
	pairs_keys_intersection(ConcAbstr, AllConcrete, UsedConcAbstr),
	transpose_pairs(UsedConcAbstr, AbstrConc),
	group_pairs_by_key(AbstrConc, Grouped),
	list_to_assoc(Grouped, AbstractMap).


%%	pairs_keys_intersection(+Pairs, +Keys, -PairsInKeys) is det.
%
%	True if PairsInKeys is a subset  of   Pairs  whose key appear in
%	Keys. Pairs must be key-sorted and Keys must be sorted.  E.g.
%
%	==
%	?- pairs_keys_intersection([a-1,b-2,c-3], [a,c], X).
%	X = [a-1,c-3]
%	==

pairs_keys_intersection(Pairs, [K], Int) :- !,	% One key: happens quite often
	find_one_key(Pairs, K, Int).
pairs_keys_intersection([P1|TP], [K1|TK], Int) :- !,
	compare_pair_key(Diff, P1, K1),
	pairs_keys_isect(Diff, P1, TP, K1, TK, Int).
pairs_keys_intersection(_, _, []).

pairs_keys_isect(<, _, [P1|TP], K1, TK, Int) :- !,
	compare_pair_key(Diff, P1, K1),
	pairs_keys_isect(Diff, P1, TP, K1, TK, Int).
pairs_keys_isect(=, P, [P1|TP], K1, TK, [P|Int]) :- !,
	compare_pair_key(Diff, P1, K1),
	pairs_keys_isect(Diff, P1, TP, K1, TK, Int).
pairs_keys_isect(>, P1, TP, _, [K1|TK], Int) :- !,
	compare_pair_key(Diff, P1, K1),
	pairs_keys_isect(Diff, P1, TP, K1, TK, Int).
pairs_keys_isect(=, P, _, _, _, [P]) :- !.
pairs_keys_isect(_, _, _, _, _, []).

compare_pair_key(Order, K1-_, K2) :- !,
	compare(Order, K1, K2).

find_one_key([], _, []).
find_one_key([K0-V|T0], K, List) :-
	(   K0 == K
	->  List = [k0-V|T],
	    find_one_key(T0, K, T)
	;   find_one_key(T0, K, List)
	).


%%	map_to_bagged_graph(+GraphIn, +Map, -GraphOut, -Bags) is det.
%
%	GraphOut is a graph between  objects   and  bags, using the most
%	specific common ancestor for representing properties.

map_to_bagged_graph(GraphIn, Map, GraphOut, Bags) :-
	map_graph(GraphIn, Map, AbstractGraph, AbstractMap),
%	assertion(map_assoc(is_ordset, AbstractMap)),
	empty_assoc(Nodes),
	rdf_to_paired_graph(GraphIn, PairGraph),
	phrase(bagify_triples(AbstractGraph, PairGraph, AbstractMap,
			      Nodes, Bags, []),
	       GraphOut).

bagify_triples([], _, _, _, Bags, Bags) --> [].
bagify_triples([rdf(S0,_P,O0)|T], PairGraph, Map, Nodes, Bags, BagsT) -->
	{ bagify_resource(S0, S, Map, Nodes, Nodes1, Bags, BagsT0),
	  bagify_object(O0, O, Map, Nodes1, Nodes2, BagsT0, BagsT1),

					% normal properties
	  used_properties(S0, O0, PairGraph, Map, PList),
	  common_ancestor_forest(sub_property_of, PList, Forest),
	  debug(used_properties, 'Forest = ~p', [Forest]),
	  pairs_keys(Forest, PRoots),
					% inverse properties
	  used_properties(O0, S0, PairGraph, Map, IPList),
	  common_ancestor_forest(sub_property_of, IPList, IForest),
	  debug(used_properties, 'IForest = ~p', [IForest]),
	  pairs_keys(IForest, IPRoots)
	},
	mk_p_triples(PRoots, S, O),
	mk_p_triples(IPRoots, O, S),
	bagify_triples(T, PairGraph, Map, Nodes2, BagsT1, BagsT).


bagify_resource(R0, R, _Map, Nodes, Nodes) -->
	{ get_assoc(R0, Nodes, R) }, !.
bagify_resource(R0, BagID, Map, Nodes0, Nodes) -->
	{ get_assoc(R0, Map, Set), Set = [_,_|_], !,
	  atom_concat('__rbag_', R0, BagID),
	  put_assoc(R0, Nodes0, BagID, Nodes)
	},
	make_rdf_graphs([BagID-Set]).
bagify_resource(R0, One, Map, Nodes, Nodes) -->
	{ get_assoc(R0, Map, [One]) }, !.
bagify_resource(R, R, _, Nodes, Nodes) --> [].

bagify_object(R0, R, Map, Nodes0, Nodes) -->
	bagify_resource(R0, R, Map, Nodes0, Nodes).


%%	rdf_to_paired_graph(+GraphIn, -PairedGraph) is det.
%
%	@param GraphIn		Graph as list(rdf(S,P,O))
%	@param PairedGraph	Graph as list(S-list(O-P)), where the
%				pair lists are key-sorted,

rdf_to_paired_graph(Triples, Pairs) :-
	subject_pairs(Triples, Pairs0),
	keysort(Pairs0, Pairs1),
	group_pairs_by_key(Pairs1, Pairs2),
	maplist(keysort_values, Pairs2, Pairs).

subject_pairs([], []).
subject_pairs([rdf(S,P,O)|T0], [S-(O-P)|T]) :-
	subject_pairs(T0, T).

keysort_values(K-V0, K-V) :-
	keysort(V0, V).


%%	used_properties(+S0, +O0, +GraphIn, +AbstractMap, -PredList) is det.
%
%	Find properties actually used between two   bags.  S0 and O0 are
%	the subject and object from the   abstract graph.
%
%	@param GraphIn	original concrete graph represented as pairs.
%			See rdf_to_paired_graph/2.
%	@param AbstractMap Assoc Abstract->Concrete, where Concrete is
%			an ordset of resources.

used_properties(S0, O0, GraphIn, Map, PList) :-
	get_assoc(S0, Map, SList),
	get_assoc(O0, Map, OList),
	pairs_keys_intersection(GraphIn, SList, Intersection),
	pairs_values(Intersection, OPList0),
	append(OPList0, OPList1),
	keysort(OPList1, OPList),
	pairs_keys_intersection(OPList, OList, IntPList),
	pairs_values(IntPList, PListDupl),
	sort(PListDupl, PList),
	debug(used_properties, '  --> ~p', [PList]).


%%	graph_resources(+Graph, -Resources:list(atom)) is det.
%
%	Resources is a sorted list  of   unique  resources  appearing in
%	Graph. All resources are in Resources,   regardless  of the role
%	played in the graph: node, edge (predicate)  or type for a typed
%	literal.
%
%	@see graph_resources/4 distinguishes the role of the resources.

graph_resources(Graph, Resources) :-
	graph_resources(Graph, R, P, P, T, T, [], _, _),
	sort(R, Resources).

%%	graph_nodes(+Graph, -Nodes) is det.
%
%	Nodes is a sorted list of   all resources and literals appearing
%	in Graph.
%
%	@tbd	Better name

graph_nodes(Graph, Nodes) :-
	graph_resources(Graph, Nodes0, P, P, L, _, _, L, []),
	sort(Nodes0, Nodes).


%%	graph_resources(+Graph,
%%			-Resources:list(atom),
%%			-Predicates:list(atom),
%%			-Types:list(atom)) is det.
%
%	Resources is a sorted list  of   unique  resources  appearing in
%	Graph as subject or object of a  triple. Predicates is a list of
%	all unique predicates in Graph and Types is a list of all unique
%	literal types in Graph.

graph_resources(Graph, Resources, Preds, Types) :-
	graph_resources(Graph, R, [], P, [], T, [], _, _),
	sort(R, Resources),
	sort(P, Preds),
	sort(T, Types).

graph_resources([], R, R, P, P, T, T, L, L).
graph_resources([rdf(S,P,O)|T], [S|RT0], RT, [P|PTl0], PTl, Tl0, Tl, L0, L) :-
	object_resources(O, RT0, RT1, Tl0, Tl1, L0, L1),
	graph_resources(T, RT1, RT, PTl0, PTl, Tl1, Tl, L1, L).


object_resources(O, R0, R, T0, T, L0, L) :-
	(   atom(O)
	->  R0 = [O|R], T0 = T, L0 = L
	;   O = literal(Val)
	->  R0 = R, L0 = [O|L],
	    (	Val = type(Type, _)
	    ->	T0 = [Type|T]
	    ;	T0 = T
	    )
	;   assertion(fail)
	).


		 /*******************************
		 *	      ABSTRACT		*
		 *******************************/

%%	abstract_graph(+GraphIn, -GraphOut, +Options) is det.
%
%	Unify GraphOut with  an  abstracted   version  of  GraphIn.  The
%	abstraction is carried out triple-by-triple.   Note  there is no
%	need to abstract all triples to the   same  level. We do however
%	need to map nodes in the graph consistently. I.e. if we abstract
%	the object of rdf(s,p,o), we must abstract the subject of rdf(o,
%	p2, o2) to the same resource.
%
%	If we want to do incremental growing   we  must keep track which
%	nodes where mapped to which resources.  Option?
%
%	We must also decide on the abstraction   level  for a node. This
%	can be based on the weight  in   the  search graph, the involved
%	properties and focus such  as  location   and  time.  Should  we
%	express this focus in the weight?
%
%	Options:
%
%	    * map_in(?Map)
%	    If present, this is the initial resource abstraction map.
%	    * map_out(-Map)
%	    Provide access to the final resource abstraction map.
%	    * bags(-Bags)
%	    If provided, _bagify_ the graph, returning the triples that
%	    define the bags in Bags. The full graph is created by
%	    appending Bags to GraphOut.
%	    * merge_concepts_with_super(+Boolean)
%	    If =true= (default), merge nodes of one is a super-concept
%	    of another.

abstract_graph(GraphIn, GraphOut, Options) :-
	map_in(Options, MapIn),
	graph_resources(GraphIn, Nodes, NT, Edges, [], _T0, _TT, NT, []),
	node_map(Nodes, MapIn, Map2, Options),
	edge_map(Edges, Map2, MapOut),
	map_out(Options, MapOut),
	(   option(bags(Bags), Options)
	->  map_to_bagged_graph(GraphIn, MapOut, GraphOut, Bags)
	;   map_graph(GraphIn, MapOut, GraphOut)
	).

map_in(Options, Map) :-
	option(map_in(Map), Options, Map),
	(var(Map) -> empty_assoc(Map) ; true).

map_out(Options, Map) :-
	option(map_out(Map), Options, _).

%%	node_map(+Nodes, +Map0, -Map, +Options) is det.
%
%	Create the abstraction map  for  the   nodes  of  the  graph. It
%	consists of two steps:
%
%	    1. Map all instances to their class, except for concepts
%	    2. If some instances are mapped to class A and others to
%	       class B, where A is a super-class of B, map all instances
%	       to class A.

node_map(Nodes, Map0, Map, Options) :-
	concepts_of(Nodes, Map0, Map1, _NewConcepts),
	(   option(merge_concepts_with_super(true), Options, true)
	->  assoc_to_values(Map1, Concepts),
	    sort(Concepts, Unique),
	    identity_map(Unique, SuperMap0),
	    find_broaders(Unique, SuperMap0, SuperMap1),
	    deref_map(SuperMap1, SuperMap),
	    map_assoc(map_over(SuperMap), Map1, Map)
	;   Map = Map1
	).

map_over(Map, V0, V) :-
	(   get_assoc(V0, Map, V1)
	->  V = V1
	;   V = V0
	).

concepts_of([], Map, Map, []).
concepts_of([R|T], Map0, Map, New) :-
	get_assoc(R, Map0, _), !,
	concepts_of(T, Map0, Map, New).
concepts_of([R|T], Map0, Map, [C|New]) :-
	concept_of(R, C),
	put_assoc(R, Map0, C, Map1),
	concepts_of(T, Map1, Map, New).

%%	identity_map(+List, -Map) is det.
%%	find_broaders(+List, +Map0, -Map) is det.
%%	deref_map(+Map0, -Map) is det.

identity_map(List, Map) :-
	map_list_to_pairs(=, List, Pairs),
	list_to_assoc(Pairs, Map).

find_broaders([], Map, Map).
find_broaders([C|T], Map0, Map) :-
	broader(C, Super),
	get_assoc(Super, Map0, SuperSuper), !,
	debug(rdf_abstract, 'Mapped ~p to super concept ~p', [C, SuperSuper]),
	put_assoc(C, Map0, SuperSuper, Map1),
	find_broaders(T, Map1, Map).
find_broaders([_|T], Map0, Map) :-
	find_broaders(T, Map0, Map).


deref_map(Map0, Map) :-
	findall(KV, mapped_kv(KV, Map0), Pairs),
	deref(Pairs, NewPairs),
	list_to_assoc(NewPairs, Map).

mapped_kv(K-V, Assoc) :-
	gen_assoc(K, Assoc, V),
	K \== V.

%%	deref(+Pairs0, NewPairs) is det.
%
%	Dereference chains V1-V2, V2-V3  into   V1-V3,  V2-V3. Note that
%	Pairs0 may contain cycles, in which case  all the members of the
%	cycle  are  replaced  by  the    representative  as  defined  by
%	rdf_representative/2.

deref(Pairs, NewPairs) :-
	list_to_assoc(Pairs, Assoc),
	deref(Pairs, Assoc, NewPairs).

deref([], _, []).
deref([K-V0|T0], Map, [K-V|T]) :-
	deref2(V0, Map, [V0], EqSet, V),
	(   EqSet == []
	->  deref(T0, Map, T)
	;   rdf_representative(EqSet, V),
	    deref_cycle(T0, EqSet, V, Cycle, T1),
	    append(Cycle, T2, T),
	    deref(T1, Map, T2)
	).

deref2(V0, Map, Visited, EqSet, V) :-
	get_assoc(V0, Map, V1), !,
	(   memberchk(V1, Visited)
	->  EqSet = Visited
	;   deref2(V1, Map, [V1|Visited], EqSet, V)
	).
deref2(V, _, _, [], V).

deref_cycle([], _, _, [], []).
deref_cycle([K-V0|T0], EqSet, V, [K-V|CT], Rest) :-
	memberchk(V0, EqSet), !,
	deref_cycle(T0, EqSet, V, CT, Rest).
deref_cycle([H|T0], EqSet, V, CT, [H|RT]) :-
	deref_cycle(T0, EqSet, V, CT, RT).


%%	edge_map(+Edges, +MapIn, -MapOut) is det.

edge_map([], Map, Map).
edge_map([R|T], Map0, Map) :-
	get_assoc(R, Map0, _), !,
	edge_map(T, Map0, Map).
%edge_map([R|T], Map0, Map) :-
%	iface_abstract_predicate(R, C),
%	put_assoc(R, Map0, C, Map1),
%	edge_map(T, Map1, Map).

%%	concept_of(+Resource, -Concept) is det.
%
%	True if Concept is the concept Resource belongs to.  If Resource
%	is a concept itself, Concept is Resource.
%
%	@tbd	Make thesaurus concept classes a subclass of skos:Class.
%	@tbd	Put in a reusable place, merge with kwd_search.pl

concept_of(O, O) :-
	rdfs_individual_of(O, skos:'Concept'), !.
concept_of(O, C) :-
	rdf_has(O, rdf:type, C), !.
concept_of(O, O).

%%	broader(+Term, -Broader) is nondet.
%
%	True if Broader is a broader term according to the SKOS schema.
%
%	@tbd Deal with owl:sameAs (and skos:exactMatch)

broader(Term, Broader) :-
	rdf_reachable(Term, skos:broader, Broader),
	Broader \== Term.

%%	rdf_representative(+Resources:list, -Representative:atom) is det.
%
%	Representative is the most popular   resource from the non-empty
%	list  Resources.  The  preferred   representative  is  currently
%	defined as the resource with the   highest  number of associated
%	edges.
%
%	@tbd	Think about the function.  Use sum of logs or sum of sqrt?

rdf_representative(List, Representative) :-
	(   exclude(rdf_is_bnode, List, NonBNodes),
	    NonBNodes \== []
	->  representative(NonBNodes, Representative)
	;   representative(List, Representative)
	).

representative([H], Representative) :- !,
	Representative = H.
representative([H|T], Representative) :-
	fan_in_out(H, Fan0),
	best(T, Fan0, H, Representative).

best([], _, R, R).
best([H|T], S0, R0, R) :-
	fan_in_out(H, S1),
	(   S1 > S0
	->  best(T, S1, H, R)
	;   best(T, S0, R0, R)
	).

fan_in_out(R, Fan) :-
	count(rdf(R, _, _), 100, FanOut),
	count(rdf(_, _, R), 100, FanIn),
	Fan is FanOut + FanIn.


		 /*******************************
		 *	      SIMPLIFY		*
		 *******************************/

%%	minimise_graph(+GraphIn, -GraphOut) is det.
%
%	Remove redudant triples from  a   graph.  Redundant  triples are
%	defined as:
%
%		* Super-properties of another property
%		* Inverse
%		* Symetric
%		* Entailed transitive
%
%	@tbd Implement entailed transitive

minimise_graph(RDF0, RDF) :-
	partition(object_triple, RDF0, ObjRDF, LitRDF),
	map_list_to_pairs(os_rdf, ObjRDF, Pairs),
	group_pairs_by_key(Pairs, Grouped),
	maplist(key_remove_reduntant_relations, Grouped, MinGroups),
	append([LitRDF|MinGroups], RDF).

object_triple(rdf(_,_,O)) :-
	atom(O).

os_rdf(rdf(S,_,O), (A+B)) :-
	(   S @< O
	->  A = S, B = O
	;   A = O, B = S
	).

key_remove_reduntant_relations(_-Rs0, Rs) :-
	remove_reduntant_relations(Rs0, Rs).

remove_reduntant_relations([R], [R]) :- !.
remove_reduntant_relations(List0, List) :-
	select(rdf(S,P1,O), List0, List1),
	select(rdf(S,P2,O), List1, List2),
	rdfs_subproperty_of(P1, P2), !,
	remove_reduntant_relations([rdf(S,P1,O)|List2], List).
remove_reduntant_relations(List0, List) :-
	select(rdf(S,P,O), List0, List1),
	select(rdf(O,P,S), List1, List2),
	rdfs_individual_of(P, owl:'SymmetricProperty'), !,
	remove_reduntant_relations([rdf(S,P,O)|List2], List).
remove_reduntant_relations(List0, List) :-
	select(rdf(S,P1,O), List0, List1),
	select(rdf(O,P2,S), List1, List2),
	rdf_has(P1, owl:inverseOf, P2), !,
	remove_reduntant_relations([rdf(S,P2,O)|List2], List).
remove_reduntant_relations(List, List).


		 /*******************************
		 *		UTIL		*
		 *******************************/

:- meta_predicate
	count(:, +, -).

count(G, Max, Count) :-
        C = c(0),
        (   G,
            arg(1, C, C0),
            C1 is C0+1,
            nb_setarg(1, C, C1),
            C1 == Max
        ->  Count = Max
        ;   arg(1, C, Count)
        ).
