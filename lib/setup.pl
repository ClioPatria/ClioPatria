/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007-2010, University of Amsterdam,
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

:- module(setup,
	  [ setup_scripts/2,		% +SrcDir, +DestDir
	    setup_default_config/3,	% +SrcDir, +DestDir, +Options
	    setup_prolog_executable/1,	% -Exec for #!
	    setup_goodbye/0,
	    copy_file_with_vars/3	% +In, +Out, +Vars
	  ]).
:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(conf_d)).
:- use_module(library(apply_macros), []).


/** <module> Configuration (setup) of ClioPatria
*/

:- multifile
	substitutions/1.

%%	setup_scripts(+SrcDir, +DstDir)
%
%	Copy all *.in files in SrcDir   into DstDir, replacing variables
%	denoted as @NAME@. Defined variables are:
%
%	    $ SWIPL :
%	    The SWI-Prolog executable as it must be used in #!
%	    $ CLIOPATRIA :
%	    Directory that holds the ClioPatria system
%	    $ CWD :
%	    The (current) installation directory
%	    $ PARENTDIR :
%	    Parent of CWD.  This can be useful if the startup-script
%	    is located in a subdirectory of a project.

setup_scripts(SrcDir, DstDir) :-
	substitutions(Vars),
	print_message(informational, setup(localize_dir(SrcDir))),
	atom_concat(SrcDir, '/*.in', Pattern),
	expand_file_name(Pattern, Files),
	maplist(install_file(Vars, DstDir), Files).

install_file(Vars, Dest, InFile) :-
	(   exists_directory(Dest)
	->  file_name_extension(File, in, InFile),
	    file_base_name(File, Base0),
	    rename_script(Base0, Base),
	    directory_file_path(Dest, Base, DstFile)
	;   DstFile = Dest
	),
	copy_file_with_vars(InFile, DstFile, Vars),
	make_runnable(DstFile),
	print_message(informational, setup(install_file(DstFile))).

%%	rename_script(+ScriptIn, -Script)
%
%	Rename scripts to satisfy the target file name association.

rename_script(Run, Script) :-
	current_prolog_flag(associate, Ext),
	file_name_extension(run, _, Run),
	file_name_extension(run, Ext, Script), !.
rename_script(Script, Script).

%%	make_runnable(+File)
%
%	Make a file executable if it starts with #!

make_runnable(File) :-
	setup_call_cleanup(
	    open(File, read, In),
	    read_line_to_codes(In, Line),
	    close(In)),
	phrase("#!", Line, _), !,
	'$mark_executable'(File).
make_runnable(_).


%%	setup_prolog_executable(?Var, ?Value)
%
%	Executable to put in #!Path. On Windows   this  is bogus, but it
%	may not contain spaces,  so  we   include  the  default Unix RPM
%	location.

setup_prolog_executable(PL) :-
	catch(getenv('SWIPL', PL), _, fail), !.
setup_prolog_executable('/usr/bin/swipl') :-
	current_prolog_flag(windows, true), !.
setup_prolog_executable(PL) :-
	current_prolog_flag(executable, Exe),
	file_base_name(Exe, Base),
	(   which(Base, PL)
	->  true
	;   PL = Exe
	).

which(File, Path) :-
	catch(getenv('PATH', SearchPath), _, fail),
	atomic_list_concat(Parts, :, SearchPath),
	member(Dir, Parts),
	directory_file_path(Dir, File, Path),
	access_file(Path, execute).

%%	setup_default_config(+ConfigEnabled, +ConfigAvail, +Options)
%
%	Setup  the  enabled  cofiguration  directory    from  the  given
%	ConfigAvail. If Options include help(true), this prints a set of
%	available options.

setup_default_config(ConfigEnabled, ConfigAvail, Options) :-
	option(help(true), Options), !,
	setup_config_help(ConfigEnabled, ConfigAvail).
setup_default_config(ConfigEnabled, ConfigAvail, Options) :-
	setup_config_enabled(ConfigEnabled, Options),
	default_config(ConfigEnabled, ConfigAvail, Options).


setup_config_enabled(ConfigEnabled, Options) :-
	(   exists_directory(ConfigEnabled)
	->  true
	;   make_directory(ConfigEnabled)
	),
	directory_file_path(ConfigEnabled, 'README.txt', Readme),
	(   exists_file(Readme)
	->  true
	;   option(readme(ReadMeIn), Options)
	->  print_message(informational,
			  setup(install_file('README.txt', ConfigEnabled))),
	    substitutions(Vars),
	    install_file(Vars, Readme, ReadMeIn)
	).

%%	default_config(+ConfigEnabledDir, +ConfigAvailDir, +Options)
%
%	Install a default configuration in ConfigEnabledDir based on the
%	information from ConfigAvailDir.  Options:
%
%	  * without(Base)
%	  Skip this file from the installation
%	  * with(Base)
%	  Add this file to the installation

default_config(ConfigEnabled, ConfigAvail, Options) :-
	directory_file_path(ConfigEnabled, 'config.done', DoneFile),
	(   exists_file(DoneFile)
	->  read_file_to_terms(DoneFile, Installed, [])
	;   Installed = []
	),
	include(with, Options, Requests),
	maplist(with_file(ConfigAvail), Requests, With),
	config_defaults(ConfigAvail, Defaults0),
	exclude(without(Options), Defaults0, Defaults),
	append(Defaults, With, Install),
	(   Install \== []
	->  setup_call_cleanup(open_done(DoneFile, Out),
			       maplist(install_config(Installed,
						      ConfigEnabled,
						      ConfigAvail,
						      Out),
				       Install),
			       close(Out))
	;   true
	).

without(Options, file(Key,_,_)) :-
	memberchk(without(Key), Options).

with(with(_)).

with_file(ConfigAvail, with(Key), file(Key, Path, link)) :-
	directory_file_path(ConfigAvail, Key, FileBase),
	absolute_file_name(FileBase, Path,
			   [ access(read),
			     file_type(prolog)
			   ]).

open_done(DoneFile, Out) :-
	exists_file(DoneFile), !,
	open(DoneFile, append, Out).
open_done(DoneFile, Out) :-
	open(DoneFile, write, Out),
	format(Out, '/* Generated file~n', []),
	format(Out, '   Keep track of installed config files~n', []),
	format(Out, '*/~n~n', []).

install_config(Installed, ConfigEnabled, ConfigAvail, Out,
	       file(_Key, File, How)) :-
	file_base_name(File, Base),
	\+ ( memberchk(file(IFile,_,_), Installed),
	     file_base_name(IFile, Base)
	   ), !,
	install_config_file(How, ConfigEnabled, File),
	get_time(Now),
	Stamp is round(Now),
	format(Out, '~q.~n', [file(Base, ConfigAvail, Stamp)]).
install_config(_, _, _, _, _).


%%	config_defaults(+ConfigAvail, -Defaults) is det.
%
%	Defaults is a list of file(Key, File, How) that indicates which
%	available config files must be installed by default.
%
%	@param ConfigAvail is either a directory or an alias.

config_defaults(ConfigAvail, Defaults) :-
	compound(ConfigAvail), !,
	findall(Defs,
		(   absolute_file_name(ConfigAvail, Dir,
				       [ file_type(directory),
					 solutions(all),
					 access(read)
				       ]),
		    config_defaults_dir(Dir, Defs)
		),
		AllDefs),
	append(AllDefs, Defaults).
config_defaults(ConfigAvail, Defaults) :-
	config_defaults_dir(ConfigAvail, Defaults).


config_defaults_dir(ConfigAvail, Defaults) :-
	directory_file_path(ConfigAvail, 'DEFAULTS', DefFile),
	access_file(DefFile, read), !,
	read_file_to_terms(DefFile, Terms, []),
	config_defaults(Terms, ConfigAvail, Defaults).
config_defaults_dir(_, []).

config_defaults([], _, []).
config_defaults([H|T0], ConfigAvail, [F|T]) :-
	config_default(H, ConfigAvail, F), !,
	config_defaults(T0, ConfigAvail, T).
config_defaults([_|T0], ConfigAvail, T) :-
	config_defaults(T0, ConfigAvail, T).


config_default((Head :- Body), ConfigAvail, File) :- !,
	call(Body),
	config_default(Head, ConfigAvail, File).
config_default(config(FileBase, How), ConfigAvail,
	       file(Key, Path, How)) :- !,
	(   File = FileBase
	;   prolog_file_type(Ext, prolog),
	    file_name_extension(FileBase, Ext, File)
	),
	directory_file_path(ConfigAvail, File, Path),
	exists_file(Path),
	file_base_name(File, Base),
	file_name_extension(Key, _, Base).
config_default(Term, _, _) :-
	domain_error(config_term, Term).


%%	setup_config_help(+ConfigEnabled, +ConfigAvail) is det.

setup_config_help(ConfigEnabled, ConfigAvail) :-
	doc_collect(true),
	config_defaults(ConfigAvail, Defaults),
	conf_d_configuration(ConfigAvail, ConfigEnabled, Configs),
	partition(default_config(Defaults), Configs, Default, NonDefault),
	maplist(config_help(without), Default, Without),
	maplist(config_help(with), NonDefault, With),
	print_message(informational, setup(without(Without))),
	print_message(informational, setup(with(With))).

default_config(Defaults, Key-_) :-
	memberchk(file(Key,_,_), Defaults).

config_help(With, Key-[Example,_], Help) :-
	(   conf_d_member_data(title, Example, Title)
	->  true
	;   Title = 'no description'
	),
	Help =.. [With,Key,Title].


%%	install_config_file(+How, +ConfDir, +File) is det.
%
%	Install  the  configuration  file  File   in  the  configuration
%	directory ConfDir. How dictates how the file is installed and is
%	one of:
%
%	  * link
%	  Link the file. This means that the configured system updates
%	  the config file if it is updated in the package.
%	  * copy
%	  Copy the file.  This is used if the config file in the package
%	  is merely a skeleton that needs to be instantiated for the
%	  specific ClioPatria installation.

install_config_file(_, ConfDir, Path) :-
	file_base_name(Path, File),
	directory_file_path(ConfDir, File, Dest),
	exists_file(Dest), !.
install_config_file(link, ConfDir, Source) :-
	file_base_name(Source, File),
	directory_file_path(ConfDir, File, Dest),
	print_message(informational, setup(install_file(File))),
	link_prolog_file(Source, Dest).
install_config_file(copy, ConfDir, Source) :-
	file_base_name(Source, File),
	directory_file_path(ConfDir, File, Dest),
	print_message(informational, setup(install_file(File))),
	copy_file(Source, Dest).

%%	link_prolog_file(+SourcePath, +DestDir) is det.
%
%	Install a skeleton file by linking it.  If it is not possible to
%	create a symbolic link (typically on  system that do not support
%	proper links such as Windows), create  a Prolog `link' file that
%	loads the target.

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

%%	setup_goodbye
%
%	Say we are done.  Waits for the user in Windows to allow the
%	user read messages.

setup_goodbye :-
	current_prolog_flag(windows, true), !,
	format(user_error, '~N~nReady.  Press any key to exit. ', []),
	get_single_char(_),
	format(' Goodbye!~n'),
	halt.
setup_goodbye :-
	halt.


		 /*******************************
		 *	       UTIL		*
		 *******************************/

%%	copy_file_with_vars(+File, +DirOrFile, +Bindings) is det.
%
%	As =|cp File DirOrFile|=, while substituting =|@var@|=
%	from Bindings using copy_stream_with_vars/3.

copy_file_with_vars(File, DirOrFile, Bindings) :-
	destination_file(DirOrFile, File, Dest),
	open(File, read, In),
	open(Dest, write, Out),
	call_cleanup(copy_stream_with_vars(In, Out, Bindings),
		     (close(In), close(Out))).

destination_file(Dir, File, Dest) :-
	exists_directory(Dir), !,
	atomic_list_concat([Dir, File], /, Dest).
destination_file(Dest, _, Dest).


%%	copy_stream_with_vars(+In:stream, +Out:stream,
%%			      +Bindings:list(Var=Name)) is det.
%
%	Copy all data from In to Out,   while replacing =|@var@|= with a
%	binding from Bindings. In addition, =|!var!|= is replaced with a
%	Prolog-quoted version of the variable content.
%
%	@param Bindings	List of Var=Name or Var(Name).  If exact case
%	match fails, the match is retried with the lowercase name.

copy_stream_with_vars(In, Out, []) :- !,
	copy_stream_data(In, Out).
copy_stream_with_vars(In, Out, Bindings) :-
	get_code(In, C0),
	copy_with_vars(C0, In, Out, Bindings).

copy_with_vars(-1, _, _, _) :- !.
copy_with_vars(0'@, In, Out, Bindings) :- !,
	insert_var(0'@, C2, In, Out, Bindings),
	copy_with_vars(C2, In, Out, Bindings).
copy_with_vars(0'!, In, Out, Bindings) :- !,
	insert_var(0'!, C2, In, Out, Bindings),
	copy_with_vars(C2, In, Out, Bindings).
copy_with_vars(C0, In, Out, Bindings) :-
	put_code(Out, C0),
	get_code(In, C1),
	copy_with_vars(C1, In, Out, Bindings).

insert_var(Mark, C2, In, Out, Bindings) :-
	get_code(In, C0),
	read_var_name(C0, In, VarNameS, C1),
	atom_codes(VarName, VarNameS),
	(   C1 == Mark,
	    var_value(VarName, Value, Bindings)
	->  (   Mark == 0'@
	    ->  format(Out, '~w', [Value])
	    ;   format(Out, '~q', [Value])
	    ),
	    get_code(In, C2)
	;   format(Out, '~c~w', [Mark, VarName]),
	    C2 = C1
	).

read_var_name(C0, In, [C0|T], End) :-
	code_type(C0, alpha), !,
	get_code(In, C1),
	read_var_name(C1, In, T, End).
read_var_name(C0, _In, [], C0).

var_value(Name, Value, Vars) :-
	memberchk(Name=Value, Vars), !.
var_value(Name, Value, Vars) :-
	Term =.. [Name,Value],
	memberchk(Term, Vars), !.
var_value(Name, Value, Vars) :-
	downcase_atom(Name, Lwr),
	Lwr \== Name,
	var_value(Lwr, Value, Vars).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message//1.

prolog:message(setup(Term)) -->
	message(Term).

message(localize_dir(SrcDir)) -->
	[ 'Localizing scripts from ~p ...'-[SrcDir] ].
message(install_file(File, Dir)) -->
	[ 'Installing ~w in ~w ...'-[File, Dir] ].
message(install_file(File)) -->
	{ file_base_name(File, Base) },
	[ ' Installing ~w ...'-[Base] ].
message(without(List)) -->
	[ nl, 'Use --without-X to disable default components' ],
	help(List).
message(with(List)) -->
	[ nl, 'Use --with-X to enable non-default components' ],
	help(List).

help([]) --> [].
help([H|T]) -->
	[nl],
	help(H),
	help(T).
help(without(Key, Title)) -->
	[ '  --without-~w~t~28|~w'-[Key, Title] ].
help(with(Key, Title)) -->
	[ '  --with-~w~t~28|~w'-[Key, Title] ].
