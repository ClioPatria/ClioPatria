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
:- if(exists_source(library(filesex))).
:- use_module(library(filesex)).
:- endif.

/** <module> Library for building installation scripts
*/

:- multifile
	setup:substitutions/1.

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
	format(user_error, 'Localizing scripts from ~w ...', [SrcDir]),
	atom_concat(SrcDir, '/*.in', Pattern),
	expand_file_name(Pattern, Files),
	maplist(install_file(Vars, DstDir), Files),
	format(user_error, ' done~n', []).

install_file(Vars, Dest, InFile) :-
	(   exists_directory(Dest)
	->  file_name_extension(File, in, InFile),
	    file_base_name(File, Base),
	    atom_concat(Dest, Base, DstFile)
	;   DstFile = Dest
	),
	copy_file_with_vars(InFile, DstFile, Vars),
	make_runnable(DstFile),
	file_base_name(DstFile, Print),
	format(user_error, ' (~w)', [Print]).

%%	make_runnable(+File)
%
%	Make a file executable if it starts with #!

make_runnable(File) :-
	open(File, read, In),
	read_line_to_codes(In, Line),
	close(In),
	append("#!", _, Line), !,
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
%	ConfigAvail.

setup_default_config(ConfigEnabled, ConfigAvail, Options) :-
	setup_config_enabled(ConfigEnabled, Options),
	default_config(ConfigEnabled, ConfigAvail).


setup_config_enabled(ConfigEnabled, Options) :-
	(   exists_directory(ConfigEnabled)
	->  true
	;   make_directory(ConfigEnabled)
	),
	directory_file_path(ConfigEnabled, 'README.txt', Readme),
	(   exists_file(Readme)
	->  true
	;   option(readme(ReadMeIn), Options)
	->  file_base_name(ConfigEnabled, Base),
	    format(user_error, 'Installing README.txt in ~w ...', [Base]),
	    substitutions(Vars),
	    install_file(Vars, Readme, ReadMeIn),
	    format(user_error, ' done~n', [])
	).

default_config(ConfigEnabled, ConfigAvail) :-
	directory_file_path(ConfigEnabled, 'config.done', DoneFile),
	(   exists_file(DoneFile)
	->  read_file_to_terms(DoneFile, Installed, [])
	;   Installed = []
	),
	(   directory_file_path(ConfigAvail, 'DEFAULTS', DefFile),
	    access_file(DefFile, read)
	->  read_file_to_terms(DefFile, Defaults, []),
	    setup_call_cleanup(open_done(DoneFile, Out),
			       maplist(install_default(Installed,
						       ConfigEnabled,
						       ConfigAvail,
						       Out),
				       Defaults),
			       close(Out))
	;   true
	).


open_done(DoneFile, Out) :-
	exists_file(DoneFile), !,
	open(DoneFile, append, Out).
open_done(DoneFile, Out) :-
	open(DoneFile, write, Out),
	format(Out, '/* Generated file~n', []),
	format(Out, '   Keep track of installed config files~n', []),
	format(Out, '*/~n~n', []).

install_default(Installed, ConfigEnabled, ConfigAvail, Out, Term) :-
	config_file(Term, ConfigAvail, File, How),
	\+ memberchk(file(File,_,_), Installed), !,
	install_file(How, ConfigEnabled, ConfigAvail, File),
	get_time(Now),
	Stamp is round(Now),
	format(Out, '~q.~n', [file(File, ConfigAvail, Stamp)]).
install_default(_, _, _, _, _).

config_file((Head:-Cond), ConfigAvail, File, How) :- !,
	call(Cond),
	config_file(Head, ConfigAvail, File, How).
config_file(config(FileBase, How), ConfigAvail, File, How) :- !,
	(   (   File = FileBase
	    ;	prolog_file_type(Ext, prolog),
		file_name_extension(FileBase, Ext, File)
	    ),
	    directory_file_path(ConfigAvail, File, Path),
	    exists_file(Path)
	->  true
	;   print_message(warning, error(existence_error(config_file, FileBase))),
	    fail
	).
config_file(Term, _, _, _) :-
	domain_error(config_term, Term).

install_file(_, ConfDir, _, File) :-
	directory_file_path(ConfDir, File, Dest),
	exists_file(Dest), !.
install_file(link, ConfDir, ConfigAvail, File) :-
	directory_file_path(ConfigAvail, File, Source),
	directory_file_path(ConfDir, File, Dest),
	format(user_error, 'Install config file ~w ...', [File]),
	try_link_file(Source, Dest, How),
	format(user_error, ' ~w~n', [How]).
install_file(copy, ConfDir, ConfigAvail, File) :-
	directory_file_path(ConfigAvail, File, Source),
	directory_file_path(ConfDir, File, Dest),
	format(user_error, 'Copying config file ~w ...', [File]),
	copy_file(Source, Dest),
	format(user_error, ' ok~n', []).

try_link_file(Source, Dest, How) :-
	relative_file_name(Source, Dest, Rel),
	catch(link_file(Rel, Dest, symbolic), Error, true),
	(   var(Error)
	->  How = linked
	;   current_prolog_flag(windows, true)
	->  copy_file(Source, Dest),
	    How = copied
	;   throw(Error)
	).


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
		 *	  COMPATIBILITY		*
		 *******************************/

:- if(\+current_predicate(link_file/3)).

link_file(From, To, symbolic) :-
	process_create(path(ln), ['-s', file(From), file(To)], []).

:- endif.

:- if(\+current_predicate(copy_file/2)).

copy_file(From, To) :-
	copy_file_with_vars(From, To, []).

:- endif.

:- if(\+current_predicate(relative_file_name/3)).

relative_file_name(Path, RelTo, RelPath) :-
	absolute_file_name(Path, AbsPath),
	absolute_file_name(RelTo, AbsRelTo),
        atomic_list_concat(PL, /, AbsPath),
        atomic_list_concat(RL, /, AbsRelTo),
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

:- if(\+current_predicate(directory_file_path/3)).

directory_file_path(Dir, File, Path) :-
	(   sub_atom(Dir, _, _, 0, /)
	->  atom_concat(Dir, File, Path)
	;   atomic_list_concat([Dir, /, File], Path)
	).

:- endif.
