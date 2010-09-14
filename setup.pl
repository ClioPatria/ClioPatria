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

%%	setup
%
%	Prepare current directory for running ClioPatria

setup :-
	install_pl_files,
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

substitutions([ 'SWIPL'=PL,		% Prolog executable (for
		'CWD'=PWD,		% This directory
		'PARENTDIR'=Parent	% The parent
	      ]) :-
	prolog_executable(PL),
	working_directory(PWD, PWD),
	file_directory_name(PWD, Parent).

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
	concat_atom(Parts, :, SearchPath),
	member(Dir, Parts),
	concat_atom([Dir, File], /, Path),
	access_file(Path, execute).

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
