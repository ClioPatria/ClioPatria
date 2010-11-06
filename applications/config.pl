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
:- use_bundle(html_page).
:- use_module(library(conf_d)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(ordsets)).
:- use_module(pldoc(doc_index)).
:- use_module(cliopatria(hooks)).
:- use_module(user(user_db)).
:- use_module(components(messages)).
:- if(exists_source(library(filesex))).
:- use_module(library(filesex)).
:- endif.

/** <module> ClioPatria configuration interface

This application provides a web-interface   for configuration management
by adding files to =|config-enabled|=.
*/

:- http_handler(cliopatria('admin/config'),	 config,      []).
:- http_handler(cliopatria('admin/reconfigure'), reconfigure, []).

cliopatria:menu_item(250=admin/config,  'Plugins').

%%	config(+Request)
%
%	HTTP handler that shows the  current   status  of  available and
%	installed configuration modules.

config(_Request) :-
	if_allowed(admin(config), [edit(true)], Options),
	reply_html_page(cliopatria(admin),
			title('Server plugin configuration'),
			[ h1('Server plugin configuration'),
			  \edit_config_table(Options),
			  \insert_html_file(html('help-config.html'))
			]).

if_allowed(Token, Options, Options) :-
	logged_on(User, anonymous),
	catch(check_permission(User, Token), _, fail), !.
if_allowed(_, _, []).

%%	edit_config_table(+Options)
%
%	HTML  Component  that  shows   the    available   and  installed
%	configuration components.

edit_config_table(Options) -->
	{ option(edit(true), Options) }, !,
	html(form([ action(location_by_id(reconfigure)),
		    method('GET')
		  ],
		  \config_table(Options))).
edit_config_table(Options) -->
	config_table(Options).

config_table(Options) -->
	{ config_files(Configs)
	},
	html(table(class(form),
		   [ \config_table_header
		   | \config_modules(Configs, 1, Options)
		   ])).

config_table_header -->
	html(tr(class(header),
		[th('Config'), th('Title'), th('Status')])).

config_modules([], _, Options) -->
	(   { option(edit(true), Options) }
	->  html(tr(class(buttons),
		    td([ colspan(3), align(right), style('padding-top:1em;')
		       ],
		       [ input(type(reset)),
			 input([type(submit),value('Update configuration')])
		       ])))
	;   []
	).
config_modules([H|T], OE, Options) -->
	{ config_module_status(H, Status) },
	odd_even_row(OE, OE1, \config_module(Status, H, Options)),
	config_modules(T, OE1, Options).

config_module_status(_-[_,-], not) :- !.
config_module_status(_-[-,_], local) :- !.
config_module_status(_-[Templ,Installed], Status) :-
	conf_d_member_data(file, Templ, TemplFile),
	conf_d_member_data(file, Installed, InstalledFile),
	compare_files(TemplFile, InstalledFile, Status).

config_module(Status, Data, Options) -->
	{ Data = Key-_Members,
	  prop_member(Status, Data, Props)
	},
	html([ td(\config_key(Key, Props)),
	       td(\config_title(Props)),
	       \config_installed(Status, Key, Options)
	     ]).

prop_member(not, _-[Templ,_], Templ) :- !.
prop_member(_,	 _-[_,Installed], Installed).


config_key(Key, Data) -->
	{ conf_d_member_data(file, Data, File),
	  doc_file_href(File, HREF)
	},
	html(a(href(HREF), Key)).

config_title(Data) -->
	{ conf_d_member_data(title, Data, Title) }, !,
	html([ Title ]).
config_title(_) -->
	html([]).

config_installed(Value, Key, Options) -->
	{ option(edit(true), Options),
	  findall(o(O,L,LC), ( option(O,L,A,LC),
			       (   Value==O
			       ->  true
			       ;   memberchk(Value, A)
			       )
			     ),
		  Pairs)
	}, !,
	html(td(class(buttons),
		select([name(Key),style('width:100%')],
		       \installed_options(Pairs, Value)))).
config_installed(Value, _, _) -->
	{ option(Value, Label, _, _)
	},
	html(td(Label)).

installed_options([], _) --> [].
installed_options([H|T], Value) -->
	installed_option(H, Value),
	installed_options(T, Value).

installed_option(o(V,L,_LC), V) -->
	html(option([value(V),selected], L)).
installed_option(o(V,_L,LC), _) -->
	html(option([value(V),class(change)], LC)).

option(not,				% Id
       'Not installed',			% Label if current status
       [linked,copied,modified],	% State that can be changed to me
       'Remove').			% Label to change
option(linked,
       'Installed (linked)',
       [not,copied,modified],
       'Link').
option(copied,
       'Installed (copied)',
       [not,linked,modified],
       'Copy').
option(modified,
       'Installed (modified)',
       [],
       '').
option(local,
       'Local',
       [],
       '').

%%	compare_files(+File, +File2, -Status) is det.
%
%	Compare  two  files,  unifying  Status  with  one  of  =linked=,
%	=copied= or =modified=.

compare_files(Templ, Installed, Status) :-
	(   same_file(Templ, Installed)
	->  Status = linked
	;   same_file_content(Templ, Installed)
	->  Status = copied
	;   Status = modified
	).

same_file_content(File1, File2) :-
	setup_call_cleanup((open(File1, read, In1),
			    open(File2, read, In2)),
			   same_stream_content(In1, In2),
			   (close(In2), close(In1))).

same_stream_content(In1, In2) :-
	get_code(In1, C1),
	get_code(In2, C2),
	same_stream_content(C1, C2, In1, In2).

same_stream_content(C, C, In1, In2) :-
	(   C == -1
	->  true
	;   same_stream_content(In1, In2)
	).


%%	config_files(-Configs)
%
%	@param	Configs is a list if Key-[Example,Installed], where
%		either is (-) or a config data item as required by
%		conf_d_member_data/3.  The list is sorted on Key.

config_files(Configs) :-
	keyed_config(cliopatria('config-available'), Templ),
	keyed_config('config-enabled', Installed),
	merge_pairlists([Templ, Installed], Configs).


keyed_config(Dir, List) :-
	conf_d_members(Dir, TemplMembers, []),
	map_list_to_pairs(key_by_file, TemplMembers, List0),
	keysort(List0, List).

key_by_file(Data, Key) :-
	conf_d_member_data(file, Data, Path),
	file_name_extension(Plain, _, Path),
	file_base_name(Plain, Key).

%%	reconfigure(+Request)
%
%	Update configuration on the basis of the menu.

reconfigure(Request) :-
	authorized(admin(reconfigure)),
	http_link_to_id(config, [], HREF),
	http_parameters(Request, [], [form_data(Form)]),
	call_showing_messages(update_config(Form),
			      [ footer(h4(['Done. ',
					   a(href(HREF),
					     'back to configuration')]))
			      ]).

update_config(Form) :-
	config_files(Configs),
	maplist(update_config_key(Form, Updated), Configs),
	(   var(Updated)
	->  print_message(informational, config(no_changes))
	;   conf_d_reload
	).

update_config_key(Form, Updated, Config) :-
	Config = Key-Versions,
	config_module_status(Config, CurrentStatus),
	(   memberchk(Key=NewStatus, Form),
	    NewStatus \== CurrentStatus
	->  update_config_file(CurrentStatus, NewStatus, Versions),
	    Updated = true
	;   true
	).

update_config_file(linked, not, [_,Installed]) :- !,
	conf_d_member_data(file, Installed, File),
	delete_file(File),
	print_message(informational, config(delete(File))).
update_config_file(_, not, [_,Installed]) :- !,
	conf_d_member_data(file, Installed, File),
	atom_concat(File, '.disabled', DisabledFile),
	catch(delete_file(DisabledFile), _, true),
	rename_file(File, DisabledFile),
	print_message(informational, config(rename(File, DisabledFile))).
update_config_file(not, linked, [Templ,_]) :-
	conf_d_member_data(file, Templ, File),
	file_base_name(File, Base),
	local_conf_dir(Dir),
	atomic_list_concat([Dir, /, Base], NewFile),
	try_link_file(File, NewFile, How, Level),
	print_message(Level, config(link(NewFile, How))).
update_config_file(copied, linked, [Templ,Installed]) :-
	conf_d_member_data(file, Templ, TemplFile),
	conf_d_member_data(file, Installed, InstalledFile),
	delete_file(InstalledFile),
	try_link_file(TemplFile, InstalledFile, How, Level),
	print_message(Level, config(link(InstalledFile, How))).
update_config_file(not, copied, [Templ,_]) :-
	conf_d_member_data(file, Templ, File),
	file_base_name(File, Base),
	local_conf_dir(Dir),
	atomic_list_concat([Dir, /, Base], NewFile),
	copy_file(File, NewFile),
	print_message(informational, config(copy(NewFile))).
update_config_file(linked, copied, [Templ,Installed]) :-
	conf_d_member_data(file, Templ, TemplFile),
	conf_d_member_data(file, Installed, InstalledFile),
	delete_file(InstalledFile),
	copy_file(TemplFile, InstalledFile),
	print_message(informational, config(copy(InstalledFile))).


try_link_file(Source, Dest, How, Level) :-
	relative_file_name(Source, Dest, Rel),
	catch(link_file(Rel, Dest, symbolic), Error, true),
	(   var(Error)
	->  How = linked,
	    Level = informational
	;   current_prolog_flag(windows, true)
	->  copy_file(Source, Dest),
	    How = copied,
	    Level = warning
	;   throw(Error)
	).


local_conf_dir(Dir) :-
	absolute_file_name('config-enabled', Dir,
			   [ file_type(directory),
			     access(write)
			   ]).


:- multifile prolog:message//1.

prolog:message(config(Action)) -->
	message(Action).

message(delete(File)) --> ['Deleted '], file(File).
message(rename(Old, New)) --> ['Renamed '], file(Old), [' into '], file(New).
message(link(File, linked)) --> ['Linked '], file(File).
message(link(File, copied)) --> ['Copied '], file(File).
message(copy(File)) --> ['Copied '], file(File).
message(no_changes) --> ['No changes; configuration is left untouched'].

file(Path) -->
	{ working_directory(Dir,Dir),
	  ensure_slash(Dir, RelTo),
	  relative_file_name(Path, RelTo, Rel)
	},
	[ '~w'-[Rel] ].

ensure_slash(Dir0, Dir) :-
	(   sub_atom(Dir0, _, _, 0, /)
	->  Dir = Dir0
	;   atom_concat(Dir0, /, Dir)
	).


		 /*******************************
		 *	       LIB		*
		 *******************************/

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
%
%	@tbd Is this useful and generic enough for library(pairs)?

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


		 /*******************************
		 *	  COMPATIBILITY		*
		 *******************************/

% library(filesex) additions added in 5.11.9

:- if(\+current_predicate(link_file/3)).

link_file(_, _, symbolic) :-
	current_prolog_flag(windows, true), !,
	domain_error(link_type, symbolic).
link_file(From, To, symbolic) :-
	process_create(path(ln), ['-s', file(From), file(To)], []).

:- endif.

:- if(\+current_predicate(copy_file/2)).

copy_file(From, To) :-
	destination_file(To, From, Dest),
	setup_call_cleanup(open(Dest, write, Out, [type(binary)]),
			   copy_from(From, Out),
			   close(Out)).

copy_from(File, Stream) :-
	setup_call_cleanup(open(File, read, In, [type(binary)]),
			   copy_stream_data(In, Stream),
			   close(In)).

destination_file(Dir, File, Dest) :-
	exists_directory(Dir), !,
	atomic_list_concat([Dir, File], /, Dest).
destination_file(Dest, _, Dest).

:- endif.

:- if(\+current_predicate(relative_file_name/3)).

relative_file_name(Path, RelTo, RelPath) :-
        atomic_list_concat(PL, /, Path),
        atomic_list_concat(RL, /, RelTo),
        delete_common_prefix(PL, RL, PL1, PL2),
        to_dot_dot(PL2, DotDot, PL1),
        atomic_list_concat(DotDot, /, RelPath).

delete_common_prefix([H|T01], [H|T02], T1, T2) :- !,
        delete_common_prefix(T01, T02, T1, T2).
delete_common_prefix(T1, T2, T1, T2).

to_dot_dot([], Tail, Tail).
to_dot_dot([_], Tail, Tail) :- !.
to_dot_dot([_|T0], ['..'|T], Tail) :-
        to_dot_dot(T0, T, Tail).

:- endif.
