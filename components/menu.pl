/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam,
                              VU University Amsterdam
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

:- module(cp_menu,
          [ cp_menu//0
          ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(uri)).
:- use_module(library(ctypes)).
:- use_module(user(user_db)).
:- use_module(cliopatria(hooks)).

/** <module> ClioPatria menu-bar

This  module  provides  the   ClioPatria    application   menu-bar.  The
application menu is attached by cliopatria(skin)  to all HTML pages that
match the style cliopatria(_) (see reply_html_page/3).

@see    The menu is built using CSS from
        http://denilsonsa.selfip.org/~denilson/menu/menu.html
*/

%!  cp_menu//
%
%   HTML Components that emits the ClioPatria   menu.  The menu is a
%   standard nested HTML =ul= list, turned   into  a horizontal menu
%   using CSS. The menu can be   extended and controlled using three
%   hooks in the module =cliopatria=:
%
%       * cliopatria:menu_item/2 defines the menu-items present
%       * cliopatria:menu_label/2 assigns non-standard labels
%       * cliopatria:menu_popup_order/2 defines the order of the popups

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
menu([_-[Item]|T]) -->
    !,
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

menu_item(item(_Rank, Spec, Label, Options)) -->
    { atom(Spec) },
    !,
    { (   \+ sub_atom(Spec, 0, _, _, 'http://'),
          catch(http_location_by_id(Spec, Location), E,
                (   print_message(informational, E),
                    fail))
      ->  true
      ;   Location = Spec
      )
    },
    html(li(a([href(Location)|Options], Label))).


%!  current_menu_item(-PopupKey, -Item) is nondet.
%
%   Enumerate the menu-items.
%
%   @param PopupKey is the id  of  a   popup.  The  label thereof is
%   computed by menu_label/3 and the ordering by menu_popup_order/2.
%   @param Item is a term item(Rank, Location, Label).

current_menu_item(Key, item(Rank, Location, Label, Options)) :-
    menu_item(Spec, DefLabel),
    rank(Spec, Rank, Where, Options),
    (   Where = Key/Location
    ->  menu_label(Location, DefLabel, Label)
    ;   Where = Location,
        Key = Location,
        menu_label(Location, DefLabel, Label)
    ).

rank(Rank=Spec, Rank, Where, Options) :-
    !,
    item_options(Spec, Where, Options).
rank(Spec,      0,    Where, Options) :-
    item_options(Spec, Where, Options).

item_options(Spec+Option, Where, [Option|T]) :-
    !,
    item_options(Spec, Where, T).
item_options(Where, Where, []).


%!  menu_item(Item, ?Label) is nondet.
%
%   Define a menu-item for  the   ClioPatria  application menu. This
%   predicate is hooked by cliopatria:menu_item/2.
%
%   @param Item is of the form Rank=Popup/Handler, where Handler is
%   the identifier of the HTTP handler (see http_handler/3).
%
%   @param Label is the label of the popup.

menu_item(Item, Label) :-
    cliopatria:menu_item(Item, Label).

menu_item(100=repository/load_file_form,                'Load local file').
menu_item(200=repository/load_url_form,                 'Load from HTTP').
menu_item(300=repository/load_library_rdf_form,         'Load from library').
menu_item(400=repository/remove_statements_form,        'Remove triples').
menu_item(500=repository/clear_repository_form,         'Clear repository').

menu_item(100=query/yasgui_editor,                      'YASGUI SPARQL Editor').
menu_item(200=query/query_form,                         'Simple Form').

menu_item(100=places/home,                              'Home').
menu_item(200=places/list_graphs,                       'Graphs').
menu_item(200=places/list_prefixes,                     'Prefixes').

menu_item(100=admin/list_users,                         'Users').
menu_item(200=admin/settings,                           'Settings').
menu_item(300=admin/statistics,                         'Statistics').

menu_item(100=user/login_form+class(login),             'Login') :-
    \+ someone_logged_on.
menu_item(100=current_user/user_logout,                 'Logout') :-
    someone_logged_on.
menu_item(200=current_user/change_password_form,        'Change password') :-
    local_user_logged_on.
menu_item(300=current_user/my_openid_page,              'My OpenID page') :-
    open_id_user(_).

sort_menu_popups(List, Sorted) :-
    map_list_to_pairs(popup_order, List, Keyed),
    keysort(Keyed, KeySorted),
    pairs_values(KeySorted, Sorted).

popup_order(Key-Members, Order-(Key-Members)) :-
    (   menu_popup_order(Key, Order)
    ->  true
    ;   Order = 550                 % between application and help
    ).

%!  menu_popup_order(+Item, -Location)
%
%   Provide numeric locations for the   popup-items.  This predicate
%   can be hooked by cliopatria:menu_popup_order/2.

menu_popup_order(Popup, Order) :-
    cliopatria:menu_popup_order(Popup, Order),
    !.
menu_popup_order(places,       100).
menu_popup_order(admin,        200).
menu_popup_order(repository,   300).
menu_popup_order(query,        400).
menu_popup_order(application,  500).
menu_popup_order(help,         600).
menu_popup_order(user,         700).
menu_popup_order(current_user, 800).

%!  menu_label(+Id, +Default, -Label) is det.

menu_label(Item, _Default, Label) :-
    cliopatria:menu_label(Item, Label),
    !.
menu_label(current_user, _Default, Label) :-
    logged_on(User, X),
    X \== User,
    !,
    (   user_property(User, realname(RealName))
    ->  true
    ;   RealName = 'My account'
    ),
    (   user_property(User, url(URL))
    ->  Label = a(href(URL), i(RealName))
    ;   Label = i(RealName)
    ).
menu_label(_, Default, Label) :-
    id_to_label(Default, Label).

%!  id_to_label(+HandlerID, -Label) is det.
%
%   Computes a default label  from   the  HandlerID. Underscores are
%   mapped to spaces and the first character is capitalised.

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

%!  local_user_logged_on is semidet.
%
%   True if the currently logged on user is a local user (as opposed
%   to an OpenID accredited logon).

local_user_logged_on :-
    logged_on(User, X),
    X \== User,
    \+ ( uri_components(User, Components),
         uri_data(scheme, Components, Scheme),
         nonvar(Scheme)
       ).

%!  someone_logged_on is semidet.
%
%   True if some user is logged on.

someone_logged_on :-
    logged_on(User, X),
    X \== User.

                 /*******************************
                 *            OpenID            *
                 *******************************/

:- http_handler(root(my_openid_page), my_openid_page, []).

my_openid_page(Request) :-
    open_id_user(User),
    http_redirect(see_other, User, Request).

open_id_user(User) :-
    logged_on(User, X),
    X \== User,
    uri_components(User, Components),
    uri_data(scheme, Components, Scheme),
    nonvar(Scheme).

