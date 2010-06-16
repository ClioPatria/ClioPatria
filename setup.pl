#!/usr/bin/pl -q -g setup -s
/*  This file is part of ClioPatria.

    Author:
    HTTP:	http://e-culture.multimedian.nl/
    GITWEB:	http://gollem.science.uva.nl/git/ClioPatria.git
    GIT:	git://gollem.science.uva.nl/home/git/ClioPatria.git
    GIT:	http://gollem.science.uva.nl/home/git/ClioPatria.git
    Copyright:  2007, E-Culture/MultimediaN

    ClioPatria is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    ClioPatria is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ClioPatria.  If not, see <http://www.gnu.org/licenses/>.
*/

:- module(cp_setup,
	  [ setup/0
	  ]).
:- load_files([ '../ClioPatria/util/files',
		library(lists)
	      ],
	      [ silent(true)
	      ]).

%%	setup
%
%	Prepare current directory for running ClioPatria

setup :-
	install_pl_files,
	install_ignore_file,
	goodbye.


%%	install_pl_files
%
%	Copy all *.pl.in to *.pl, while replacing @var@ constructs and
%	make the result executable.
%	
%	@tbd	Provide public interface for '$mark_executable'/1.

install_pl_files :-
	substitutions(Vars),
	format(user_error, 'Localizing scripts ...', []),
	expand_file_name('*.pl.in', Files),
	maplist(install_pl_file(Vars), Files),
	format(user_error, ' done~n', []).

install_pl_file(Vars, InFile) :-
	file_name_extension(PlFile, in, InFile),
	copy_file_with_vars(InFile, PlFile, Vars),
	'$mark_executable'(PlFile),
	file_base_name(PlFile, Base),
	format(user_error, ' (~w)', [Base]).

substitutions(['PL'=PL]) :-
	prolog_executable(PL).

%%	prolog_executable(-Path)
%
%	Executable to put in #!Path. On Windows   this  is bogus, but it
%	may not contain spaces,  so  we   include  the  default Unix RPM
%	location.

prolog_executable(PL) :-
	catch(getenv('PL', PL), _, fail), !.
prolog_executable('/usr/bin/pl') :-
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
	concat_atom(Parts, :, SearchPath),
	member(Dir, Parts),
	concat_atom([Dir, File], /, Path),
	access_file(Path, execute).

%%	install_ignore_file
%
%	Install an ignore file

install_ignore_file :-
	ignore_file(File),
	(   exists_file(File)
	->  true
	;   format(user_error, 'Installing ~w ...', [File]),
	    copy_file_with_vars('gitignore.in', File, []),
	    format(user_error, ' done~n', [])
	).

ignore_file('.cvsignore') :-
	exists_directory('CVS'), !.
ignore_file('.gitignore').
	

		 /*******************************
		 *	     RUN IT		*
		 *******************************/

goodbye :-
	current_prolog_flag(windows, true), !,
	format(user_error, '~N~nReady.  Press any key to exit. ', []),
	get_single_char(_),
	format(' Goodbye!~n'),
	halt.
goodbye :-
	halt.
