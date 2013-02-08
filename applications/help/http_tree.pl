/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010-2013, VU University Amsterdam

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

:- module(http_tree,
	  [ http_tree_view//1
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/yui_resources)).
:- use_module(library(option)).
:- use_module(library(pairs)).

/** <module> Create YUI tree from HTTP locations

This module provides the  component   http_tree_view//1  and  associated
helpers.
*/

:- http_handler(root(help/expand_http_node), expand_http_node, []).

%%	http_tree_view(+Options)//
%
%	Show hierarchy of HTTP locations (paths).  The tree is a YUI
%	tree that can be expanded dynamically.

http_tree_view(Options) -->
	tree_view(expand_http_node, Options).


% the folders/tree.css file must be last.  Because requirements are made
% unique and sorted using toplogical sort, this  can only be achieved by
% declaring the other files as dependencies.

:- html_resource(yui_examples('treeview/assets/css/folders/tree.css'),
		 [ requires([ yui('treeview/treeview.js'),
			      yui('connection/connection.js'),
			      yui('treeview/assets/skins/sam/treeview.css')
			    ])
		 ]).


tree_view(Handler, Options) -->
	{ http_location_by_id(Handler, Path),
	  TreeViewID = treeDiv1
	},
	html([ \html_requires(yui_examples('treeview/assets/css/folders/tree.css')),

	       div(id(TreeViewID), []),
	       \tree_view_script(Path, TreeViewID, Options)
	     ]).

tree_view_script(Path, TreeViewID, Options) -->
	html(script(type('text/javascript'), \[
'var currentIconMode = 0;\n',		% ??
'function buildTree() {\n',
'   tree = new YAHOO.widget.TreeView("~w");\n'-[TreeViewID],
'   tree.setDynamicLoad(loadNodeData, currentIconMode);\n',
\tree_options(Options),
'   var root = tree.getRoot();\n',
'\n',
'   var tempNode = new YAHOO.widget.TextNode("/", root, true);\n',
'   tempNode.data = { path:"/" };\n',
'   tree.draw();\n',
'}\n',

'function loadNodeData(node, fnLoadComplete)  {\n',
'    var sUrl = "~w?node=" + encodeURIComponent(node.data.path);\n'-[Path],
'    var callback = {\n',
'        success: function(oResponse) {\n',
'	     var children = eval(oResponse.responseText);\n',
'	     for (var i=0, j=children.length; i<j; i++) {\n',
'		 var tempNode = new YAHOO.widget.TextNode(children[i], node, false);\n',
'            }\n',
'            oResponse.argument.fnLoadComplete();\n',
'        },\n',
'        failure: function(oResponse) {\n',
'            oResponse.argument.fnLoadComplete();\n',
'        },\n',
'        argument: {\n',
'            "node": node,\n',
'            "fnLoadComplete": fnLoadComplete\n',
'        },\n',
'        timeout: 7000\n',
'    };\n',
'    YAHOO.util.Connect.asyncRequest("GET", sUrl, callback);\n',
'}\n',

%'YAHOO.util.Event.onDOMReady(buildTree());\n'
'buildTree();\n'
					     ])).

tree_options([]) --> [].
tree_options([H|T]) --> tree_option(H), tree_options(T).

tree_option(labelClick(JS)) --> !,
	html([ 'tree.subscribe("labelClick", ~w);\n'-[JS] ]).
tree_option(_) -->
	[].

%%	expand_http_node(+Request)
%
%	HTTP handler that returns the children of an HTTP node.

expand_http_node(Request) :-
	http_parameters(Request,
			[ node(Parent, [ description('HTTP location to refine')])
			]),
	node_children(Parent, Children),
	reply_json(Children, []).

node_children(Parent, Children) :-
	ensure_ends_slash(Parent, Parent1),
	findall(Sub, sub_handler(Parent1, Sub), Subs),
	map_list_to_pairs(first_component(Parent), Subs, Keyed0),
	keysort(Keyed0, Keyed),
	group_pairs_by_key(Keyed, Groups),
	maplist(decorate, Groups, Children).

ensure_ends_slash(Path, Path) :-
	sub_atom(Path, _, _, 0, /), !.
ensure_ends_slash(Path, PathSlash) :-
	atom_concat(Path, /, PathSlash).

sub_handler(Parent, Sub) :-
	http_current_handler(Sub, _:_, _),
	sub_atom(Sub, 0, _, A, Parent),
	A > 0.

first_component(Parent, Path, ParentExt) :-
	atom_length(Parent, PL),
	sub_atom(Path, B, _, _, /),
	B > PL, !,
	sub_atom(Path, 0, B, _, ParentExt).
first_component(_, Path, Path).


decorate(Prefix-[Only],
	 json([label(Label), isLeaf(@(true)), path(Only)])) :-
	atom_concat(Prefix, Rest, Only),
	(   Rest == ''
	;   Rest == /
	), !,
	file_base_name(Prefix, Label0),
	leaf_label(Only, Label0, Label).
decorate(Prefix-_,
	 json([label(Label), isLeaf(@(false)), path(Prefix)])) :-
	file_base_name(Prefix, Label).

leaf_label(Only, Label0, Label) :-
	http_current_handler(Only, _:_, Options),
	(   memberchk(prefix(true), Options)
	->  atom_concat(Label0, '...', Label)
	;   Label = Label0
	).



