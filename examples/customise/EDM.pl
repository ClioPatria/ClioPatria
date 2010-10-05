:- module(edm,
	  [
	  ]).
:- use_module(cliopatria(hooks)).
:- use_module(library(uri)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_abstract)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_path)).
:- use_module(library(lists)).
:- use_module(components(label)).
:- use_module(library(settings)).
:- use_module(library(count)).

/** <module> Customise ClioPatria for EDM

This module customises ClioPatria  for  use   with  EDM  (Europeana Data
Model) data. It realises two customizations:

    * When displaying a link to a WebResource, it either uses the
    referenced image as label or shows both the internal link and
    a link to the external resource.

    * Context graphs are customised, both in how they are computed and
    in the rendering of the EDM classes.

@see cliopatria(hooks) for a description of the hooks.
*/

:- rdf_register_ns(ens, 'http://www.europeana.eu/schemas/edm/').
:- rdf_register_ns(ore, 'http://www.openarchives.org/ore/terms/').

%%	cliopatria:display_link(+URI, +Options)// is det.
%
%	Display EDM WebResources.  If  the   resource  is  a  thumbnail,
%	display it inline. Else put an icon behind the default link that
%	links to the external site.

cliopatria:display_link(R, Options) -->
	{ \+ memberchk(edm(false), Options),
	  rdf_has(R, rdf:type, ens:'WebResource')
	},
	(   { content_type(R, Type),
	      sub_atom(Type, 0, _, _, 'image/')
	    }
	->  html(a(href(R), img([class(thumbnail), src(R)])))
	;   { http_absolute_location(icons('external-link-ltr-icon.png'),
				     External, [])
	    },
	    html(span([ \rdf_link(R, [edm(false)|Options]),
			html(a([class(img), href(R)], img(src(External))))
		      ]))
	).


:- dynamic
	content_type_cache/2.

content_type(URL, MimeType) :-
	content_type_cache(URL, Type), !,
	atom(Type),
	Type = MimeType.
content_type(URL, MimeType) :-
	uri_components(URL, Components),
	uri_data(scheme, Components, Scheme),
	Scheme == http,
	catch(http_open(URL, Stream,
			[ method(head),
			  header(content_type, Type)
			]), _, fail),
	close(Stream),
	assertz(content_type_cache(URL, Type)),
	MimeType = Type.
content_type(URL, _) :-
	assertz(content_type_cache(URL, meta(fail))),
	fail.


		 /*******************************
		 *	       CONTEXT		*
		 *******************************/

% Use SVG context graphs, so we can include images.

:- set_setting_default(graphviz:format, svg).

%%	cliopatria:context_graph(+URI, -Graph)
%
%	Compute the EDM context graph. This is currently defined to do a
%	two-step breadth-first expansion of the graph from URI using the
%	known EDM properties. Branching from a single node is limited to
%	20 and the total graph is not expanded beyond 100 nodes.

:- rdf_meta
	edm_context(r).

cliopatria:context_graph(R, RDF) :-
	bf_graph(R, 2, 100, 20, RDF0),
	minimise_graph(RDF0, RDF1),		% remove inverse/symmetric/...
	bagify_graph(RDF1, RDF2, Bags, []), 	% Create bags of similar resources
	append(RDF2, Bags, RDF).

%%	bf_graph(+Start, +MaxDist, +MaxNodes, -Graph)

bf_graph(Start, MaxDist, MaxEdges, MaxBranch, Graph) :-
	bf_graph_2([Start,+], MaxDist, MaxEdges, MaxBranch, 0, _, [], Graph).

bf_graph_2([], _, _, _, D, D, G, G) :- !.
bf_graph_2(_, MaxDist, _, _, D, D, G, G) :-
	D >= MaxDist, !.
bf_graph_2(AG0, MaxDist, MaxEdges, MaxBranch, D0, D, G0, G) :-
	bf_expand(AG0, AG, D0, D1, MaxBranch, G1),
	(   G1 == []
	->  bf_graph_2(AG, MaxDist, MaxEdges, MaxBranch, D1, D, G0, G)
	;   append(G1, G0, G2),
	    sort(G2, G3),
	    length(G3, Edges),
	    (   Edges >= MaxEdges
	    ->  D = D0,
		G = G0
	    ;   bf_graph_2(AG, MaxDist, MaxEdges, MaxBranch, D1, D, G3, G)
	    )
	).

bf_expand([+|AG], AG, D0, D, _, []) :- !,
	D is D0 + 1.
bf_expand([F|AG0], AG, D, D, MaxBranch, Triples) :-
	answer_set(Dst-Triple, related(F, Dst, Triple), MaxBranch, Pairs),
	pairs_keys_values(Pairs, Dsts, Triples),
	append(AG0, Dsts, AG).

related(S, O, rdf(S,P,O)) :-
	edm_context(Rel),
	rdf_has(S, Rel, O, P).
related(O, S, rdf(S,P,O)) :-
	edm_context(Rel),
	rdf_has(S, Rel, O, P).

edm_context(ore:aggregates).
edm_context(ore:proxyFor).
edm_context(ore:proxyIn).
edm_context(ens:aggregatedCHO).
edm_context(ens:hasThumbnail).
edm_context(dcterms:hasPart).
edm_context(ens:isNextInSequence).
edm_context(ens:object).		% this is *always* a thumbnail
edm_context(ens:hasView).


%%	cliopatria:node_shape(+URI, -Shape, +Options)
%
%	Realise   EDM-specific   vizualisation   of     nodes   in   the
%	context-graph.

cliopatria:node_shape(URI, Shape, Options) :-
	memberchk(start(URI), Options),
	Shape = [shape(tripleoctagon),style(filled),fillcolor('#ff85fd')].
cliopatria:node_shape(URI, Shape, _Options) :-
	rdf_has(URI, rdf:type, ens:'WebResource'),
	content_type(URI, Type),
	sub_atom(Type, 0, _, _, 'image/'),
	Shape = [img([src(URI)])].
cliopatria:node_shape(URI, Shape, _Options) :-
	rdf_has(URI, rdf:type, ore:'Aggregation'),
	Shape = [shape(box3d),style(filled),fillcolor('#85fff7')].
cliopatria:node_shape(URI, Shape, _Options) :-
	rdf_has(URI, rdf:type, ore:'Proxy'),
	Shape = [shape(diamond),style('rounded,filled'),fillcolor('#ffb785')].
cliopatria:node_shape(URI, Shape, _Options) :-
	rdf_has(URI, rdf:type, ens:'PhysicalThing'),
	Shape = [shape(house),style('filled'),fillcolor('#ff8585')].
