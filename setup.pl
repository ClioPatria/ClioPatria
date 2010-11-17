:- module(cp_setup,
	  [ setup/0
	  ]).
:- load_files('lib/setup', [silent(true)]).

:- multifile
	user:file_search_path/2.
:- dynamic
	user:file_search_path/2.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(cliopatria, Dir)).

:- initialization
	set_prolog_flag(verbose, normal),
	setup.

%%	setup
%
%	Setup ClioPatria. This installs files   *.in from the ClioPatria
%	after localization and creates config-enabled.

setup :-
	cliopatria_dir(ClioDir),
	working_directory(CWD, CWD),
	setup_scripts(ClioDir, CWD),
	directory_file_path(ClioDir, 'lib/APPCONF.txt.in', ReadmeIn),
	directory_file_path(ClioDir, 'config-available', ConfigAvail),
	directory_file_path(CWD,     'config-enabled', ConfigEnabled),
	setup_default_config(ConfigEnabled, ConfigAvail,
			     [ readme(ReadmeIn)
			     ]),
	setup_goodbye.

cliopatria_dir(Dir) :-
	absolute_file_name(cliopatria(.),
			   Dir,
			   [ file_type(directory),
			     access(read)
			   ]).



setup:substitutions([ 'SWIPL'=PL,		% Prolog executable (for #!...)
		      'CLIOPATRIA'=ClioDir,	% ClioPatria directory
		      'CWD'=CWD,		% This directory
		      'PARENTDIR'=Parent	% Parent of CWD
		    ]) :-
	cliopatria_dir(ClioDir),
	working_directory(CWD, CWD),
	file_directory_name(CWD, Parent),
	setup_prolog_executable(PL).


directory_file_path(Dir, File, Path) :-
	(   sub_atom(Dir, _, _, 0, /)
	->  atom_concat(Dir, File, Path)
	;   atomic_list_concat([Dir, /, File], Path)
	).
