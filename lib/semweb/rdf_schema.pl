:- module(rdf_schema,
	  [ make_schema/2		% +DataGraph, +SchemaGraph
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

%%	make_schema(+Graph, +SchemaGraph) is det.
%
%	Create an initial  schema  by   providing  definitions  for  all
%	predicates and types (classes) used  in   Graph.  The  schema is
%	dumped into the graph SchemaGraph.
%
%	This  predicate  is  typically   used    _after_   running   the
%	rewrite-rules to reflect renamed typed and properties.

make_schema(Data, Schema) :-
	rdf_retractall(_,_,_,Schema),
	rdf_transaction(make_schema_(Data, Schema), make_schema).

make_schema_(Data, Schema) :-
	forall(predicate_in_graph(Data, P),
	       define_predicate(P, Data, Schema)),
	forall(type_in_graph(Data, Class),
	       define_type(Class, Schema)).

define_predicate(P, _, _) :-
	rdf_global_id(rdf:_, P), !.
define_predicate(P, _, _) :-
	rdf_global_id(rdfs:_, P), !.
define_predicate(P, DataGraph, Graph) :-
	copy_data(P, Graph),
	rdf_assert(P, rdf:type, rdf:'Property', Graph),
	assign_label(P, Graph),
	predicate_statistics(DataGraph, P, _C,
			     _Subjects, _Objects,
			     Domains, Ranges),
	(   Domains = [Dom]
	->  rdf_assert(P, rdfs:domain, Dom, Graph)
	;   true
	),
	(   Ranges = [Range]
	->  rdf_assert(P, rdfs:range, Range, Graph)
	;   true
	).


define_type(C, Graph) :-
	copy_data(C, Graph),
	rdf_assert(C, rdf:type, rdfs:'Class', Graph),
	assign_label(C, Graph).


assign_label(S, Graph) :-
	(   rdf(S, rdfs:label, _)
	->  true
	;   rdfs_label(S, Label),
	    Label \== S
	->  rdf_assert(S, rdfs:label, literal(Label), Graph)
	;   true
	).


copy_data(S, Graph) :-
	rdf_retractall(S,_,_,Graph),
	forall((rdf(S,P,O,G), G \== Graph),
	       rdf_assert(S,P,O,Graph)).


		 /*******************************
		 *	        QUERY		*
		 *******************************/

predicate_in_graph(Graph, P) :-
	rdf_current_predicate(P),
	once(rdf(_,P,_,Graph)).

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


subject_in_graph(Graph, S) :-
	rdf_subject(S),
	once(rdf(S, _, _, Graph)).

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
