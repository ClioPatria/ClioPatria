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

:- http_handler(cliopatria('admin/configuration'), configuration, []).
:- http_handler(cliopatria('admin/reconfigure'),   reconfigure,	  []).

cliopatria:menu_item(250=admin/configuration,  'Plugins').

%%	configuration(+Request)
%
%	HTTP handler that shows the  current   status  of  available and
%	installed configuration modules.

configuration(_Request) :-
	authorized(admin(config)),
	reply_html_page(cliopatria(admin),
			title('Server plugin configuration'),
			[ h1('Server plugin configuration'),
			  \edit_config_table([edit(true)]),
			  \insert_html_file(html('help-config.html'))
			]).

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
	;   link_file(Installed)
	->  Status = linked
	;   same_file_content(Templ, Installed)
	->  Status = copied
	;   Status = modified
	).

link_file(File) :-
	setup_call_cleanup(open(File, read, In),
			   read_line_to_codes(In, Line),
			   close(In)),
	atom_codes('/* Linked config file */', Line).

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
%	Get the current configuration status.

config_files(Configs) :-
	conf_d_configuration(config_available(.),
			     'config-enabled',
			     Configs).


%%	reconfigure(+Request)
%
%	Update configuration on the basis of the menu.

reconfigure(Request) :-
	authorized(admin(reconfigure)),
	http_link_to_id(configuration, [], HREF),
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
	link_prolog_file(File, NewFile),
	print_message(informational, config(link(NewFile))).
update_config_file(copied, linked, [Templ,Installed]) :-
	conf_d_member_data(file, Templ, TemplFile),
	conf_d_member_data(file, Installed, InstalledFile),
	delete_file(InstalledFile),
	link_prolog_file(TemplFile, InstalledFile),
	print_message(informational, config(link(InstalledFile))).
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


%%	link_prolog_file(+SourcePath, +DestDir) is det.
%
%	Install a skeleton file by linking it.  If it is not possible to
%	create a symbolic link (typically on  system that do not support
%	proper links such as Windows), create  a Prolog `link' file that
%	loads the target.
%
%	@see	copied from library(setup). Do not alter without
%		synchronising.

link_prolog_file(Source, Dest) :-
	relative_file_name(Source, Dest, Rel),
	catch(link_file(Rel, Dest, symbolic), Error, true),
	(   var(Error)
	->  true
	;   catch(create_link_file(Dest, Rel), E2, true)
	->  (   var(E2)
	    ->	true
	    ;	throw(E2)
	    )
	;   throw(Error)
	).

%%	create_link_file(+Dest, +Rel) is det.
%
%	Creat a _|link file|_ for a Prolog file. Make sure to delete the
%	target first, to avoid an accidental   write  through a symbolic
%	link.

create_link_file(Dest, Rel) :-
	(   access_file(Dest, exist)
	->  delete_file(Dest)
	;   true
	),
	setup_call_cleanup(open(Dest, write, Out),
			   ( format(Out, '/* Linked config file */~n', []),
			     format(Out, ':- ~q.~n', [consult(Rel)])
			   ),
			   close(Out)).


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
message(link(File)) --> ['Linked '], file(File).
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


