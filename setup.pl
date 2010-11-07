#!/usr/bin/swipl -q -g setup -s
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

:- module(cp_setup,
	  [ setup/0
	  ]).
:- use_module(library(apply)).
:- if(exists_source(library(filesex))).
:- use_module(library(filesex)).
:- endif.

/** <module> ClioPatria installation script
*/

:- multifile
	user:file_search_path/2.
:- dynamic
	user:file_search_path/2.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(cliopatria, Dir)).


%%	setup
%
%	Prepare current directory for running ClioPatria

setup :-
	absolute_file_name(cliopatria(.),
			   ClioDir,
			   [ file_type(directory),
			     access(read)
			   ]),
	working_directory(CWD, CWD),
	setup(ClioDir, CWD),
	goodbye.


%%	setup(SourceDir, DestDir)
%
%	Copy all *.pl.in to *.pl, while replacing @var@ constructs and
%	make the result executable.
%
%	@tbd	Provide public interface for '$mark_executable'/1.

setup(SrcDir, DstDir) :-
	localize_scripts(SrcDir, DstDir),
	create_conf_d(SrcDir, DstDir).

%%	localize_scripts(+SrcDir, +DstDir)
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

localize_scripts(SrcDir, DstDir) :-
	substitutions(SrcDir, DstDir, Vars),
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


substitutions(SrcDir, DstDir,
	      [ 'SWIPL'=PL,		% Prolog executable (for #!...)
		'CLIOPATRIA'=SrcDir,	% ClioPatria directory
		'CWD'=DstDir,		% This directory
		'PARENTDIR'=Parent	% Parent of CWD
	      ]) :-
	prolog_executable(PL),
	file_directory_name(DstDir, Parent).

%%	prolog_executable(-Path)
%
%	Executable to put in #!Path. On Windows   this  is bogus, but it
%	may not contain spaces,  so  we   include  the  default Unix RPM
%	location.

prolog_executable(PL) :-
	catch(getenv('SWIPL', PL), _, fail), !.
prolog_executable('/usr/bin/swipl') :-
	current_prolog_flag(windows, true), !.
prolog_executable(PL) :-
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

%%	create_conf_d(+ClioDir, +DstDir)
%
%	Create a new configure directory if it doesn't exist yet.

create_conf_d(ClioDir, DstDir) :-
	create_conf_enabled(ClioDir, DstDir, ConfEnabled),
	directory_file_path(ClioDir, 'config-available', TemplDir),
	default_config(ConfEnabled, TemplDir).


create_conf_enabled(ClioDir, DstDir, ConfDir) :-
	directory_file_path(DstDir, 'config-enabled', ConfDir),
	(   exists_directory(ConfDir)
	->  true
	;   make_directory(ConfDir)
	),
	directory_file_path(ConfDir, 'README.txt', Readme),
	(   exists_file(Readme)
	->  true
	;   format(user_error, 'Localizing config-enabled ...', []),
	    atom_concat(ClioDir, '/lib/APPCONF.txt.in', ReadmeIn),
	    substitutions(ClioDir, DstDir, Vars),
	    install_file(Vars, Readme, ReadmeIn),
	    format(user_error, ' done~n', [])
	).

default_config(ConfDir, TemplateDir) :-
	directory_file_path(ConfDir, 'config.done', File),
	(   exists_file(File)
	->  read_file_to_terms(File, Installed, [])
	;   Installed = []
	),
	(   directory_file_path(TemplateDir, 'DEFAULTS', DefFile),
	    access_file(DefFile, read)
	->  read_file_to_terms(DefFile, Defaults, []),
	    setup_call_cleanup(open(File, append, Out),
			       maplist(install_default(Installed,
						       ConfDir,
						       TemplateDir,
						       Out),
				       Defaults),
			       close(Out))
	;   true
	).


install_default(Installed, ConfDir, TemplateDir, Out, Term) :-
	config_file(Term, TemplateDir, File, How),
	\+ memberchk(file(File,_), Installed),
	install_file(How, ConfDir, TemplateDir, File),
	get_time(Now),
	format('~q.', [file(File, Now)], Out).
install_default(_, _, _, _, _).

config_file((Head:-Cond), TemplateDir, File, How) :- !,
	call(Cond),
	config_file(Head, TemplateDir, File, How).
config_file(config(FileBase, How), TemplateDir, File, How) :- !,
	(   (   File = FileBase
	    ;	prolog_file_type(Ext, prolog),
		file_name_extension(FileBase, Ext, File)
	    ),
	    directory_file_path(TemplateDir, File, Path),
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
install_file(link, ConfDir, TemplateDir, File) :-
	directory_file_path(TemplateDir, File, Source),
	directory_file_path(ConfDir, File, Dest),
	format(user_error, 'Install config file ~w ...', [File]),
	try_link_file(Source, Dest, How),
	format(user_error, ' ~w~n', [How]).
install_file(copy, ConfDir, TemplateDir, File) :-
	directory_file_path(TemplateDir, File, Source),
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

directory_file_path(Dir, File, Path) :-
	(   sub_atom(Dir, _, _, 0, /)
	->  atom_concat(Dir, File, Path)
	;   atomic_list_concat([Dir, /, File], Path)
	).


%%	goodbye
%
%	Say we are done

goodbye :-
	current_prolog_flag(windows, true), !,
	format(user_error, '~N~nReady.  Press any key to exit. ', []),
	get_single_char(_),
	format(' Goodbye!~n'),
	halt.
goodbye :-
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
%	Copy all data from In to Out, while replacing =|@var@|=
%	with a binding from Bindings.
%
%	@param Bindings	List of Var=Name

copy_stream_with_vars(In, Out, []) :- !,
	copy_stream_data(In, Out).
copy_stream_with_vars(In, Out, Bindings) :-
	get_code(In, C0),
	copy_with_vars(C0, In, Out, Bindings).

copy_with_vars(-1, _, _, _) :- !.
copy_with_vars(0'@, In, Out, Bindings) :- !,
	get_code(In, C0),
	read_var_name(C0, In, VarNameS, C1),
	atom_codes(VarName, VarNameS),
	(   C1 == 0'@,
	    memberchk(VarName=Value, Bindings)
	->  write(Out, Value),
	    get_code(In, C2)
	;   format(Out, '@~w', [VarName]),
	    C2 = C1
	),
	copy_with_vars(C2, In, Out, Bindings).
copy_with_vars(C0, In, Out, Bindings) :-
	put_code(Out, C0),
	get_code(In, C1),
	copy_with_vars(C1, In, Out, Bindings).

read_var_name(C0, In, [C0|T], End) :-
	code_type(C0, alpha), !,
	get_code(In, C1),
	read_var_name(C1, In, T, End).
read_var_name(C0, _In, [], C0).


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
