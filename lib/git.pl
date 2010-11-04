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

:- module(git,
	  [ git/2			% +Argv, +Options
	  ]).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(option)).

/** <module> Run GIT commands
*/

%%	git(+Argv, +Options) is det.
%
%	Run a GIT command.  Defined options:
%
%	  * directory(+Dir)
%	  Execute in the given directory
%	  * output(-Out)
%	  Unify Out with a list of codes representing stdout of the
%	  command.  Otherwise the output is handed to print_message/2
%	  with level =informational=.
%	  * error(-Error)
%	  As output(Out), but messages are printed at level =error=.

git(Argv, Options) :-
	option(directory(Dir), Options, .),
	setup_call_cleanup(process_create(path(git), Argv,
                                          [ stdout(pipe(Out)),
                                            stderr(pipe(Error)),
                                            process(PID),
                                            cwd(Dir)
                                          ]),
                           (   read_stream_to_codes(Out, OutCodes, []),
			       read_stream_to_codes(Error, ErrorCodes, []),
                               process_wait(PID, Status)
                           ),
			   (   close(Out),
			       close(Error)
			   )),
	print_error(ErrorCodes, Options),
	print_output(OutCodes, Options),
	Status = exit(0).

print_output(OutCodes, Options) :-
	option(output(Codes), Options), !,
	Codes = OutCodes.
print_output(OutCodes, _) :-
	print_message(informational, git(output(OutCodes))).

print_error(OutCodes, Options) :-
	option(error(Codes), Options), !,
	Codes = OutCodes.
print_error(OutCodes, _) :-
	print_message(error, git(output(OutCodes))).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message//1.

prolog:message(git(output(Codes))) -->
	{ split_lines(Codes, Lines) },
	git_lines(Lines).

git_lines([]) --> [].
git_lines([H|T]) -->
	[ '~s'-[H] ],
	(   {T==[]}
	->  []
	;   [nl], git_lines(T)
	).

split_lines([], []) :- !.
split_lines(All, [Line1|More]) :-
	append(Line1, [0'\n|Rest], All), !,
	split_lines(Rest, More).
split_lines(Line, [Line]).
