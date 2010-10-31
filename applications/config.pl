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

:- module(cpa_config, []).
:- use_package(html_page).
:- use_module(library(conf_d)).
:- use_module(library(pairs)).
:- use_module(library(ordsets)).

/** <module> ClioPatria configuration interface
*/

:- http_handler(cliopatria('admin/config'), config, []).

config(_Request) :-
	reply_html_page(cliopatria(admin),
			title('Server configuration'),
			[ h1('Server configuration'),
			  \config_table
			]).


config_table -->
	{ config_files(Configs)
	},
	html(table(class(block),
		   [ \config_table_header
		   | \config_modules(Configs, 1)
		   ])).

config_table_header -->
	html(tr([th('Config'), th('Title'), th('Status')])).

config_modules([], _) --> [].
config_modules([H|T], OE) -->
	odd_even_row(OE, OE1, \config_module(H)),
	config_modules(T, OE1).

config_module(Key-[Templ,-]) -->
	html(tr([ td(Key),
		  td(\config_name(Templ)),
		  td('Not installed')
		])).
config_module(Key-[-,Installed]) -->
	html(tr([ td(Key),
		  td(\config_name(Installed)),
		  td('Local')
		])).
config_module(Key-[Templ,_Installed]) -->
	html(tr([ td(Key),
		  td(\config_name(Templ)),
		  td('Installed')
		])).

config_name(Data) -->
	{ conf_d_member_data(title, Data, Title) }, !,
	html([ Title ]).
config_name(_) -->
	html([]).


config_files(Configs) :-
	keyed_config(cliopatria('examples/conf.d'), Templ),
	keyed_config('conf.d', Installed),
	merge_pairlists([Templ, Installed], Configs).


keyed_config(Dir, List) :-
	conf_d_members(Dir, TemplMembers, []),
	map_list_to_pairs(key_by_file, TemplMembers, List0),
	keysort(List0, List).

key_by_file(Data, Key) :-
	conf_d_member_data(file, Data, Path),
	file_name_extension(Plain, _, Path),
	file_base_name(Plain, Key).

%%	merge_pairlists(+PairLists, -Merged)
%
%	PairLists is a list of lists  of   K-V  pairs.  Merged is a K-VL
%	list, where each VL is  a  list   of  values  on K in PairLists.
%	Missing values are returned as (-).  For example:
%
%	  ==
%	  ?- merge_pairlists([ [a-1, d-4],
%			       [a-1, c-3],
%			       [b-2]
%			     ], Merged).
%	  Merged = [a-[1,1,-], b-[-,-,2], d-[4,-,-], c-[-,3,-]].
%	  ==

merge_pairlists(Lists, Merged) :-
	heads(Lists, Heads),
	sort(Heads, Sorted),
	merge_pairlists(Sorted, Lists, Merged).

heads([], []).
heads([[K-_|_]|T0], [K|T]) :- !,
	heads(T0, T).
heads([[]|T0], T) :-
	heads(T0, T).

merge_pairlists([], _, []).
merge_pairlists([K|T0], Lists, [K-Vs|T]) :-
	take_key(Lists, K, NewLists, NewKsUnsorted, Vs),
	sort(NewKsUnsorted, NewKs),
	ord_union(T0, NewKs, Ks),
	merge_pairlists(Ks, NewLists, T).

take_key([], _, [], [], []).
take_key([List|T0], K, NewLists, NewKs, Vs) :-
	(   List = [KH-V|ListT],
	    KH == K
	->  NewLists = [ListT|T],
	    Vs = [V|Vs1],
	    (	ListT = [NewK-_|_]
	    ->	NewKs = [NewK|NewKs1]
	    ;	NewKs1 = NewKs
	    ),
	    take_key(T0, K, T, NewKs1, Vs1)
	;   NewLists = [List|T],
	    Vs = [(-)|Vs1],
	    take_key(T0, K, T, NewKs, Vs1)
	).


