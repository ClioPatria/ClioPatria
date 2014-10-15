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

:- module(cliopatria, []).

/** <module> ClioPatria hooks

This module declares the _hooks_ an application  may define to extend or
modify some of ClioPatria's  behaviour.   Hooks  are =multifile= defined
predicates that -typically- have no default   definition. Code using the
hook typically first calls the hook. If   the  hook succeeds the task is
considered  done.  Otherwise  some  default  action  is  performed.  For
example, a property myprefix:componentName can be   added  as a property
that provides a label using this code:

    ==
    :- use_module(cliopatria(hooks)).

    rdf_label:label_property(myprefix:componentName).
    ==

The example below adds an item to =Help= popup of ClioPatria:

    ==
    :- use_module(cliopatria(hooks)).
    :- use_module(library(http/http_dispatch)).

    cliopatria:menu_item(help/about, 'About').

    :- http_handler(root(about), about, []).

    about(Request) :-
	<generate the about page>
    ==
*/

:- multifile
	menu_item/2,
	menu_label/2,
	menu_popup_order/2,

	rdf_label:label_property/1,
	bnode_label//1,			% +Resource
	display_link//2,		% +RDFObject, +Options
	resource_link/2,		% +URI, -URL

	list_resource//2,		% +URI, +Options

	user_preference_db/2,		% ?Property, ?Value
	user_preference_default/2,	% ?Property, ?Value

	page_body//1,			% +Body
	page_body//2,			% +Style, +Body
	server_address//0,
	logo//0,

	predicate_order/2,		% +P, -Order
	context_graph/2,		% +R, -RDF
	context_graph/3,		% +R, -RDF, +Options
	node_shape/3.			% +R, -Shape, +Options


		 /*******************************
		 *	     THE MENU		*
		 *******************************/

%%	menu_item(?Item, ?Label)
%
%	This hook adds an item to the ClioPatria menu. Item is a term of
%	the form [rank=]popup/item, where _popup_   denotes  the name of
%	the popup menu and _item_ is a  (new)   item  to be added to the
%	popup. The _item_ is the handler-identifier  of the HTTP handler
%	that implements the item  (see   http_handler/3).  Label  is the
%	label displayed. _rank_ defines the   position inside the popup.
%	The built-in items are numbered 100,200,...
%
%	For example, if we want to add   a  new item to the *Repository*
%	menu after *|Load from library|* that   crawls  LOD data, we can
%	use the following code:
%
%	==
%	:- use_module(cliopatria(hooks)).
%	:- use_module(library(http/http_dispatch)).
%
%	:- handler(cliopatria('crawl_lod_form'), crawl_lod_form, []).
%
%	cliopatria:menu_item(400=repository/crawl_lod_form, 'Crawl LOD').
%
%	crawl_lod_form(Request) :-
%		...
%	==
%
%	@see The menu_label/2 and menu_popup_order/2 hooks provide
%	further control over the menu.
%	@see cp_menu:menu_item/2 implements the default menu.

%%	menu_label(+Id, -Label)
%
%	This hook allows for dynamic   or redefined (e.g., multilingual)
%	labels.  It  is   called   both    for   popup-identifiers   and
%	item-identifiers.

%%	menu_popup_order(+Id, -Location:integer)
%
%	This hook controls the order of the popup-item of ClioPatria's
%	menu.


		 /*******************************
		 *		LABELS		*
		 *******************************/

%%	rdf_label:label_property(?Property)
%
%	True if the value of  Property   can  be  used to (non-uniquely)
%	describe an object to the user.   This  hook provides additional
%	facts to cp_label:label_property/1.

%%	bnode_label(+Resource)//
%
%	HTML-write DCG rule that produces an HTML description for the
%	given RDF blank node.  See cp_label:bnode_label//1.

%%	display_link(+RDFObject)//
%
%	HTML-write DCG rule that produces an   HTML  description for the
%	given RDFObject (a resource  or   literal)  with  an appropriate
%	link. This predicate is called by the RDF browser to present RDF
%	triples.

%%	resource_link(+URI, -URL)//
%
%	URL is the link created by rdf_link//1 for URI. The default
%	opens the ClioPatria `local view'.
%
%	@see	cpa_browse:list_resource/1 is the handler addressed by the
%		default link.
%	@see	cp_label:resource_link/2 calls the hook.


		 /*******************************
		 *	    LOCAL VIEW		*
		 *******************************/

%%	list_resource(+URI, +Options)//
%
%	This  hook  is  called   by  cpa_browse:list_resource//2,  which
%	display the `local view' page for a   resource. This can be used
%	to create a different page for   describing a resource and still
%	using overall infrastructure such as rdf_link//1.


		 /*******************************
		 *   USER/SESSION PREFERENCES	*
		 *******************************/

%%	user_preference_db(?Property:atom, ?Value:rdf_object) is nondet.
%
%	Query properties for the current   user/session.  This mechanism
%	allows code to access information about the user/session without
%	committing to a particular  implementation.   The  predicate and
%	values are compatible with RDF to   allow  implementing the user
%	database in RDF, typically using the OpenID as subject.

%%	user_preference_default(?Property:atom, ?Value:rdf_object) is nondet.
%
%	Provides defaults for the user_preference/2.
%
%	@see user_preference_db/2


		 /*******************************
		 *	       SKINS		*
		 *******************************/

%%	page_body(+Body)// is semidet.
%%	page_body(+Style, +Body)// is semidet.
%
%	Emit the body of  the  page.  This   can  be  used  to provide a
%	different skin for ClioPatria. The Style argument is passed from
%	reply_html_page/3. The file skin(cliopatria) defines the overall
%	skin and first calls  cliopatria:page_body//2,   if  this  fails
%	cliopatria:page_body//1 and if  this  fails   too  it  uses  the
%	default page.

%%	server_address//
%
%	HTML-write DCG rule that writes the   address  of the server. If
%	you want to maintain its  normal   position  in the page layout,
%	this should create an element of class =address= using the class
%	=cliopatria=.

%%	logo//
%
%	Logo placed left of the menu-bar.  Must   be  an object that has
%	`float:left` style.


		 /*******************************
		 *	    RDF BROWSING	*
		 *******************************/

%%	predicate_order(+Pred, -Order) is semidet.
%
%	Define the order in which predicates appear in the local view.
%	The Order is an integer. The system ordering is defined by
%	cpa_browse:p_order/2. Predicates that are not explicitly ordered
%	are placed at the end of the table an ordered alphabetically.
%
%	Predicates that have order `0' are _deleted_ from the table.

%%	context_graph(+R, -RDF, +Options) is semidet.
%
%	@deprecated Use context_graph/3.

%%	context_graph(+R, -RDF, +Options) is semidet.
%
%	This hook redefines the context graph   shown by the RDF browser
%	for the resource R. RDF is  a   list  of rdf(S,P,O) triples that
%	describe the context. Typically only   object-triples  are used,
%	although that is not a requirement.
%
%	@see	This predicate hooks cpa_browse:context_graph/2.  Please
%		visit the soure to learn about available building
%		blocks.



%%	node_shape(+URI, -Shape, +Options) is semidet.
%
%	Compute the desired shape for a GraphViz node representing URI.
%
%	@param URI is the resource for which to determine the shape
%	@param Shape is a list Name(Value) for parameters given to
%	       GraphViz.
%	@param Options provides additional guidance. Currently it
%	       may provide start(StartURI) to indicate the graph is a
%	       context node for the resource StartURI.
%	@see   http://www.graphviz.org/doc/info/shapes.html

