:- module(prophier, []).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(assoc)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(components(label)).	% Get rdf_link//1
:- use_module(cliopatria(hooks)).	% Declaration to extend the menu

/** <module> Vizualize the RDF property hierarchy

This program demonstrates simple data   processing and vizualization. In
order to process the request, we

    1. compute the hierarchy as a Prolog datastructure (using
    property_tree/1). This allows for reuse, for example emitting the
    same datastructure as JSON, so we can do the rendering in Javscript
    at the client side.

    2. emit the tree as a nested =ul= structure

Finally, we can add it to the  ClioPatria   menu  by adding a clause for
cliopatria:menu_item/2.

@tbd	Add style to make it look pretty.
*/

% Make our predicate respond to /prophier

:- http_handler(root(prophier), property_hierarchy, []).

% add our application to the Places  menu.   300  is the location in the
% menu (check cp_menu:menu_item/2 for the initial   menu). places is the
% popup and property_hierarchy is the identifier  of our handlers, which
% defaults to the predicate name. See http_handler/3 for details.

cliopatria:menu_item(300=places/property_hierarchy, 'Predicate tree').

%%	property_hierarchy(+Request)
%
%	HTTP Handler that emits the RDF   property hierarchy as a nested
%	=ul= tree where the properties are links to the ClioPatria local
%	view.

property_hierarchy(_Request) :-
	property_tree(Tree),
	reply_html_page(cliopatria(default),
			title('Property hierarchy'),
			[ h1('RDF Property hierarchy'),
			  \emit_tree(Tree)
			]).


emit_tree([]) --> !.
emit_tree(List) -->
	html(ul(\emit_children(List))).

emit_children([]) --> [].
emit_children([node(P,Children)|T]) -->
	html(li([ \rdf_link(P)		% Create link to local view
		| \emit_tree(Children)
		])),
	emit_children(T).


%%	property_tree(-List) is det.
%
%	Compute the entire property hierarchy for the RDF database. Most
%	of the complication is due to the fact that we need to take care
%	of possible loops in the property   hierarchy. For this purpose,
%	we use library(assoc) to maintain an   binary tree of predicates
%	we already expanded.
%
%	@param List is a list of terms node(Predicate, Children)

property_tree(List) :-
	empty_assoc(Done0),
	findall(node(P, _), p_root(P), List),
	children(List, Done0, _Done).

p_root(P) :-
	rdf_current_predicate(P),
	\+ rdf_has(P, rdfs:subPropertyOf, _).

children([], Done, Done).
children([node(P, Children)|T], Done0, Done) :-
	(   get_assoc(P, Done0, _)	% Already in the tree
	->  Done = Done0
	;   put_assoc(P, Done0, true, Done1),
	    findall(node(P2, _), rdf_has(P2, rdfs:subPropertyOf, P), Children),
	    children(T, Done1, Done2),
	    children(Children, Done2, Done)
	).
