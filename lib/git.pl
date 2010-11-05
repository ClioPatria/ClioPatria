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
	  [ git/2,			% +Argv, +Options
	    git_tags_on_branch/3	% +Dir, +Branch, -Tags
	  ]).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(option)).
:- use_module(library(http/dcg_basics)).

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

%%	git_tags_on_branch(+Dir, +Branch, -Tags) is det.
%
%	Tags is a list of tags in Branch on the GIT repository Dir, most
%	recent tag first.
%
%	@see Git tricks at http://mislav.uniqpath.com/2010/07/git-tips/

git_tags_on_branch(Dir, Branch, Tags) :-
	setup_call_cleanup(process_create(path(git),
					  [ log, '--oneline', '--decorate',
					    Branch
					  ],
                                          [ stdout(pipe(Out)),
                                            process(PID),
                                            cwd(Dir)
                                          ]),
                           (   log_to_tags(Out, Tags),
                               process_wait(PID, Status)
                           ),
			   close(Out)),
	(   Status = exit(0)
	->  true
	;   throw(error(process_error(git, Status)))
	).

log_to_tags(Out, Tags) :-
	read_line_to_codes(Out, Line0),
	log_to_tags(Line0, Out, Tags, []).

log_to_tags(end_of_file, _, Tags, Tags) :- !.
log_to_tags(Line, Out, Tags, Tail) :-
	phrase(tags_on_line(Tags, Tail1), Line),
	read_line_to_codes(Out, Line1),
	log_to_tags(Line1, Out, Tail1, Tail).

tags_on_line(Tags, Tail) -->
	string_without(" ", _Hash),
	tags(Tags, Tail),
	skip_rest.

tags(Tags, Tail) -->
	whites,
	"(tag: ",
	string(Codes),
	")", !,
	{ atom_codes(Tag, Codes),
	  Tags = [Tag|Rest]
	},
	tags(Rest, Tail).
tags(Tags, Tags) -->
	skip_rest.

skip_rest(_,_).


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
