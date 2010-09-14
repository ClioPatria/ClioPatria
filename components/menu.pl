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

:- module(cp_menu,
	  [ cp_menu//0
	  ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(ctypes)).
:- use_module(user(user_db)).
:- use_module(cliopatria(hooks)).

/** <module> ClioPatria menu-bar

*/

%%	cp_menu//
%
%	Emit the ClioPatria menu.  The menu is built from three hooks in
%	the =cliopatria= namespace:
%
%	    * cliopatria:menu_item/2
%	    * cliopatria:menu_label/2
%	    * cliopatria:menu_popup_order/2

cp_menu -->
	{ findall(Key-Item, current_menu_item(Key, Item), Pairs0),
	  sort(Pairs0, Pairs),
	  group_pairs_by_key(Pairs, ByKey),
	  sort_menu_popups(ByKey, Menu)
	},
	html_requires(css('menu.css')),
	html(ul(id(nav),
		\menu(Menu))).

menu([]) --> !.
menu([_-[Item]|T]) --> !,
	menu_item(Item),
	menu(T).
menu([Key-Items|T]) -->
	{ menu_label(Key, Key, Label) },
	html(li([ a([Label]),
		  ul(\menu_items(Items))
		])),
	menu(T).

menu_items([]) --> [].
menu_items([H|T]) --> menu_item(H), menu_items(T).

menu_item(item(Spec, Label)) -->
	{ atom(Spec) }, !,
	{ (   \+ sub_atom(Spec, 0, _, _, 'http://'),
	      catch(http_location_by_id(Spec, Location), E,
		    (   print_message(informational, E),
			fail))
	  ->  true
	  ;   Location = Spec
	  )
	},
	html(li(a([href(Location)], Label))).


current_menu_item(Key, item(Location, Label)) :-
	menu_item(Where, DefLabel),
	(   Where = Key/Location
	->  menu_label(Location, DefLabel, Label)
	;   Where = Location,
	    Key = Location,
	    menu_label(Location, DefLabel, Label)
	).

%%	menu_item(?Id, ?Label) is nondet.
%
%	True if Id/Label must appear in the side-menu.  Id is one of
%
%	    * handler-id
%	    * HTML code

menu_item(Item, Label) :-
	cliopatria:menu_item(Item, Label).
menu_item(file/load_file_form,	 	'Load local file').
menu_item(file/load_url_form,		'Load from HTTP').
menu_item(file/load_library_ontology_form, 'Load ontology from library').
menu_item(file/remove_statements_form,  'Remove statements').
menu_item(file/clear_repository_form,	'Clear the repository').
menu_item(query/query_form,		'Query').
menu_item(view/list_graphs,		'Graphs').
menu_item(view/statistics,		'Statistics').
menu_item(admin/list_users,		'Users').
menu_item(admin/settings,		'Settings').
% Home location
menu_item(help/home,			'Home').
menu_item(help/cliopatria_doc,		'Documentation').
% Keep users at the end
menu_item(user/login_form,		'Login') :-
	\+ someone_logged_on.
menu_item(current_user/user_logout,	'Logout') :-
	someone_logged_on.
menu_item(current_user/change_password_form,	'Change password') :-
	someone_logged_on.

sort_menu_popups(List, Sorted) :-
	map_list_to_pairs(popup_order, List, Keyed),
	keysort(Keyed, KeySorted),
	pairs_values(KeySorted, Sorted).

popup_order(Key-Members, Order-(Key-Members)) :-
	(   menu_popup_order(Key, Order)
	->  true
	;   Order = 10000
	).

%%	menu_popup_order(+Item, -Location)
%
%	Provide numeric locations for the popup-items.

menu_popup_order(Popup, Order) :-
	cliopatria:menu_popup_order(Popup, Order), !.
menu_popup_order(file,	       1000).
menu_popup_order(query,	       2000).
menu_popup_order(view,	       3000).
menu_popup_order(admin,	       4000).
menu_popup_order(application,  5000).
menu_popup_order(help,	       6000).
menu_popup_order(user,	       7000).
menu_popup_order(current_user, 8000).

menu_label(Item, _Default, Label) :-
	cliopatria:menu_label(Item, Label), !.
menu_label(current_user, _Default, Label) :-
	logged_on(User, X),
	X \== User, !,
	(   user_property(User, realname(RealName))
	->  true
	;   RealName = User
	),
	(   user_property(User, url(URL))
	->  Label = a(href(URL), i(RealName))
	;   Label = i(RealName)
	).
menu_label(_, Default, Label) :-
	id_to_label(Default, Label).

id_to_label(Atom, Capital) :-
	atom_codes(Atom, Codes0),
	maplist(underscore_to_space, Codes0, Codes),
	(   maplist(is_upper, Codes)
	->  Capital = Atom
	;   Codes = [First|Rest]
	->  code_type(First, to_lower(Up)),
	    UpCodes = [Up|Rest],
	    atom_codes(Capital, UpCodes)
	;   Capital = Atom
	).

underscore_to_space(0'_, 32) :- !.
underscore_to_space(X, X).

someone_logged_on :-
	logged_on(User, X),
	X \== User.

