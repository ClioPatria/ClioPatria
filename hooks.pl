/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam,
                              VU University Amsterdam,
                              CWI, Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
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
    bnode_label//1,                 % +Resource
    display_link//2,                % +RDFObject, +Options
    resource_link/2,                % +URI, -URL

    list_resource//2,               % +URI, +Options

    user_preference_db/2,           % ?Property, ?Value
    user_preference_default/2,      % ?Property, ?Value

    page_body//1,                   % +Body
    page_body//2,                   % +Style, +Body
    server_address//0,
    logo//0,

    predicate_order/2,              % +P, -Order
    context_graph/2,                % +R, -RDF
    context_graph/3,                % +R, -RDF, +Options
    context_predicate/2,            % +R, -Pred
    node_label/4,                   % +R, +Lang, +MaxLen, -Label
    node_shape/3,                   % +R, -Shape, +Options
    bag_shape/3,                    % +Members, -Shape, +Options
    edge_shape/3.                   % +P, -Shape, +Options


                 /*******************************
                 *           THE MENU           *
                 *******************************/

%!  menu_item(?Item, ?Label)
%
%   This hook adds an item to the ClioPatria menu. Item is a term of
%   the form [rank=]popup/item, where _popup_   denotes  the name of
%   the popup menu and _item_ is a  (new)   item  to be added to the
%   popup. The _item_ is the handler-identifier  of the HTTP handler
%   that implements the item  (see   http_handler/3).  Label  is the
%   label displayed. _rank_ defines the   position inside the popup.
%   The built-in items are numbered 100,200,...
%
%   For example, if we want to add   a  new item to the *Repository*
%   menu after *|Load from library|* that   crawls  LOD data, we can
%   use the following code:
%
%   ==
%   :- use_module(cliopatria(hooks)).
%   :- use_module(library(http/http_dispatch)).
%
%   :- handler(cliopatria('crawl_lod_form'), crawl_lod_form, []).
%
%   cliopatria:menu_item(400=repository/crawl_lod_form, 'Crawl LOD').
%
%   crawl_lod_form(Request) :-
%           ...
%   ==
%
%   @see The menu_label/2 and menu_popup_order/2 hooks provide
%   further control over the menu.
%   @see cp_menu:menu_item/2 implements the default menu.

%!  menu_label(+Id, -Label)
%
%   This hook allows for dynamic   or redefined (e.g., multilingual)
%   labels.  It  is   called   both    for   popup-identifiers   and
%   item-identifiers.

%!  menu_popup_order(+Id, -Location:integer)
%
%   This hook controls the order of the popup-item of ClioPatria's
%   menu.


                 /*******************************
                 *              LABELS          *
                 *******************************/

%!  rdf_label:label_property(?Property)
%
%   True if the value of  Property   can  be  used to (non-uniquely)
%   describe an object to the user.   This  hook provides additional
%   facts to cp_label:label_property/1.

%!  bnode_label(+Resource)//
%
%   HTML-write DCG rule that produces an HTML description for the
%   given RDF blank node.  See cp_label:bnode_label//1.

%!  display_link(+RDFObject)//
%
%   HTML-write DCG rule that produces an   HTML  description for the
%   given RDFObject (a resource  or   literal)  with  an appropriate
%   link. This predicate is called by the RDF browser to present RDF
%   triples.

%!  resource_link(+URI, -URL)//
%
%   URL is the link created by rdf_link//1 for URI. The default
%   opens the ClioPatria `local view'.
%
%   @see    cpa_browse:list_resource/1 is the handler addressed by the
%           default link.
%   @see    cp_label:resource_link/2 calls the hook.


                 /*******************************
                 *          LOCAL VIEW          *
                 *******************************/

%!  list_resource(+URI, +Options)//
%
%   This  hook  is  called   by  cpa_browse:list_resource//2,  which
%   display the `local view' page for a   resource. This can be used
%   to create a different page for   describing a resource and still
%   using overall infrastructure such as rdf_link//1.


                 /*******************************
                 *   USER/SESSION PREFERENCES   *
                 *******************************/

%!  user_preference_db(?Property:atom, ?Value:rdf_object) is nondet.
%
%   Query properties for the current   user/session.  This mechanism
%   allows code to access information about the user/session without
%   committing to a particular  implementation.   The  predicate and
%   values are compatible with RDF to   allow  implementing the user
%   database in RDF, typically using the OpenID as subject.

%!  user_preference_default(?Property:atom, ?Value:rdf_object) is nondet.
%
%   Provides defaults for the user_preference/2.
%
%   @see user_preference_db/2


                 /*******************************
                 *             SKINS            *
                 *******************************/

%!  page_body(+Body)// is semidet.
%!  page_body(+Style, +Body)// is semidet.
%
%   Emit the body of  the  page.  This   can  be  used  to provide a
%   different skin for ClioPatria. The Style argument is passed from
%   reply_html_page/3. The file skin(cliopatria) defines the overall
%   skin and first calls  cliopatria:page_body//2,   if  this  fails
%   cliopatria:page_body//1 and if  this  fails   too  it  uses  the
%   default page.

%!  server_address//
%
%   HTML-write DCG rule that writes the   address  of the server. If
%   you want to maintain its  normal   position  in the page layout,
%   this should create an element of class =address= using the class
%   =cliopatria=.

%!  logo//
%
%   Logo placed left of the menu-bar.  Must   be  an object that has
%   `float:left` style.


                 /*******************************
                 *          RDF BROWSING        *
                 *******************************/

%!  predicate_order(+Pred, -Order) is semidet.
%
%   Define the order in which predicates appear in the local view.
%   The Order is an integer. The system ordering is defined by
%   cpa_browse:p_order/2. Predicates that are not explicitly ordered
%   are placed at the end of the table an ordered alphabetically.
%
%   Predicates that have order `0' are _deleted_ from the table.

%!  context_graph(+R, -RDF, +Options) is semidet.
%
%   @deprecated Use context_graph/3.

%!  context_graph(+R, -RDF, +Options) is semidet.
%
%   This hook redefines the context graph   shown by the RDF browser
%   for the resource R. RDF is  a   list  of rdf(S,P,O) triples that
%   describe the context. Typically only   object-triples  are used,
%   although that is not a requirement.
%
%   @see    This predicate hooks cpa_browse:context_graph/3.  Please
%           visit the soure to learn about available building
%           blocks.

%!  context_predicate(+Subject, -Predicate) is nondet.
%
%   True when rdf(Subject, Predicate, _)  must   be  included in the
%   context graph for Subject.

%!  node_label(+URI, +Lang, +MaxLen, -Label) is semidet.
%
%   Compute a label for URI in the given language, truncating the
%   label to MaxLen characters.

%!  node_shape(+URI, -Shape, +Options) is semidet.
%
%   Compute the desired shape for a GraphViz node representing URI.
%
%   @param URI is the resource for which to determine the shape
%   @param Shape is a list Name(Value) for parameters given to
%          GraphViz.
%   @param Options provides additional guidance. Currently it
%          may provide start(StartURI) to indicate the graph is a
%          context node for the resource StartURI.
%   @see   http://www.graphviz.org/doc/info/shapes.html

%!  bag_shape(+Bag, -Shape, +Options) is semidet.
%
%   Compute the desired properties for a table used to display a bag
%   of resources.  Shape options include:
%
%     - max(Max)
%     Only show the first Max members of the bag.  Default is 5.
%     - shape(Shape)
%     Basic shape
%     - style(Style)
%     Style options
%     - max_label_length(Chars)
%     Truncate labels that have more then Chars characters.
%
%   @param Bag is a list of member resources

%!  edge_shape(+Triple, -Shape, +Options) is semidet.
%
%   Compute the desired attributes for a GraphViz edge representing
%   the predicate Triple.
%
%   @param Triple is the triple for which to determine the shape
%   @param Shape is a list Name(Value) for parameters given to
%          GraphViz.
%   @param Options provides additional guidance. Currently it
%          may provide start(StartURI) to indicate the graph is a
%          context node for the resource StartURI.
