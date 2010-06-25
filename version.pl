/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2010, University of Amsterdam,
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

:- module(prolog_version,
	  [ check_prolog_version/1,	% +NumericVersion
	    git_version/2,		% -VersionID, +TagPattern
	    git_version/1		% -VersionID
	  ]).
:- use_module(library(process)).
:- use_module(library(readutil)).

/** <module> Manage software versions

*/


%%	check_prolog_version(+Required)
%
%	Validate the program is running under Prolog version Required or
%	newer. Required is in numeric notation (e.g. 50317 for 5.3.17)

check_prolog_version(Required) :-
        current_prolog_flag(version, MyVersion),
        (   MyVersion >= Required
        ->  true
        ;   print_message(error,
			  required_prolog_version(Required)),
	    format(user_error, '~nPress any key to exit> ', []),
	    get_single_char(_), nl(user_error),
	    halt(1)
        ).

:- multifile
	prolog:message/3.

prolog:message(required_prolog_version(Required)) -->
	{ current_prolog_flag(version, MyVersion),
	  user_version(MyVersion, MyV),
	  user_version(Required, Req)
	},
	[ 'This program requires SWI-Prolog ~w'-[Req], nl,
	  'while you are running version ~w.'-[MyV], nl,
	  'Please visit http://www.swi-prolog.org and', nl,
	  'upgrade your version of SWI-Prolog.'
	].


user_version(N, Version) :-
        Major is N // 10000,
        Minor is (N // 100) mod 100,
        Patch is N mod 100,
        atomic_list_concat([Major, Minor, Patch], '.', Version).


		 /*******************************
		 *	    GIT VERSION		*
		 *******************************/

:- multifile
	git_version_pattern/1.

git_version_pattern('V*').
git_version_pattern('*').

%%	git_version(-Version, +Options) is semidet.
%
%	Describe the running version  based  on   GIT  tags  and hashes.
%	Options:
%
%	    * match(+Pattern)
%	    * directory(Dir)
%
%	@see git describe

git_version(Version, Options) :-
	(   option(match(Pattern), Options)
	->  true
	;   git_version_pattern(Pattern)
	),
	option(directory(Dir), Options, .),
	process_create(path(git), ['describe', '--match', Pattern],
		       [ stdout(pipe(Out)),
			 stderr(null),
			 process(PID),
			 cwd(Dir)
		       ]),
	read_stream_to_codes(Out, V0, []),
	process_wait(PID, Status),
	Status = exit(0),
	atom_codes(V1, V0),
	normalize_space(atom(Plain), V1),
	(   git_is_clean(Dir)
	->  Version = Plain
	;   atom_concat(Plain, '-DIRTY', Version)
	).


git_is_clean(Dir) :-
	process_create(path(git), ['diff'],
		       [ stdout(pipe(Out)),
			 stderr(null),
			 cwd(Dir)
		       ]),
	setup_call_cleanup(open_null_stream(Null),
			   (   copy_stream_data(Out, Null),
			       character_count(Null, Count)
			   ),
			   close(Null)),
	Count == 0.


		 /*******************************
		 *	      UPDATE		*
		 *******************************/

:- multifile
	version_dir/1.
:- dynamic
	version_dir/1,
	version_dir/2.

:- prolog_load_context(directory, Dir),
   assert(version_dir(Dir)).

git_update_versions :-
	forall(version_dir(Dir),
	       update_version(Dir)).

update_version(Dir) :-
	git_version(GitVersion, [directory(Dir)]),
	retractall(version_dir(Dir, _)),
	assert(version_dir(Dir, GitVersion)).

%%	git_version(-Version)
%
%	Provides the version of the first valid directory.
%
%	@see git_version/2.

git_version(Version) :-
	version_dir(_, Version).


		 /*******************************
		 *	  KEEP UP-TO-DATE	*
		 *******************************/

:- multifile
	user:message_hook/3.

user:message_hook(make(done(_)), _, _) :-
	catch(git_update_versions, _, fail),
	fail.

:- initialization
	catch(git_update_versions, _, fail).
