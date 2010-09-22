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
	    register_git_component/2,	% +Name, +Options
	    git_component_property/2	% ?Name, ?Property
	  ]).
:- use_module(library(process)).
:- use_module(library(option)).
:- use_module(library(readutil)).


/** <module> Manage software versions

The module deals with software  versions.   It  currently implements two
features:  test  whether   SWI-Prolog   is    sufficiently   new   using
check_prolog_version/1 and find GIT version   signatures for the running
server. Modules that want  their  version   info  available  through the
web-page can do so using a call to register_git_component/2.
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
prolog:message(git(no_version)) -->
	[ 'Sorry, cannot retrieve version stamp from GIT.' ].


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

%%	git_describe(-Version, +Options) is semidet.
%
%	Describe the running version  based  on   GIT  tags  and hashes.
%	Options:
%
%	    * match(+Pattern)
%	    Only use tags that match Pattern (a Unix glob-pattern; e.g.
%	    =|V*|=)
%	    * directory(Dir)
%	    Provide the version-info for a directory that is part of
%	    a GIT-repository.
%
%	@see git describe

git_describe(Version, Options) :-
	(   option(match(Pattern), Options)
	->  true
	;   git_version_pattern(Pattern)
	),
	option(directory(Dir), Options, .),
	setup_call_cleanup(process_create(path(git), ['describe', '--match', Pattern],
					  [ stdout(pipe(Out)),
					    stderr(null),
					    process(PID),
					    cwd(Dir)
					  ]),
			   (   read_stream_to_codes(Out, V0, []),
			       process_wait(PID, Status)
			   ),
			   close(Out)),
	Status = exit(0),
	atom_codes(V1, V0),
	normalize_space(atom(Plain), V1),
	(   git_is_clean(Dir)
	->  Version = Plain
	;   atom_concat(Plain, '-DIRTY', Version)
	).


%%	git_is_clean(+Dir) is semidet.
%
%	True if the given directory is in   a git module and this module
%	is clean. To us, clean only   implies that =|git diff|= produces
%	no output.

git_is_clean(Dir) :-
	setup_call_cleanup(process_create(path(git), ['diff'],
					  [ stdout(pipe(Out)),
					    stderr(null),
					    cwd(Dir)
					  ]),
			   stream_char_count(Out, Count),
			   close(Out)),
	Count == 0.

stream_char_count(Out, Count) :-
	setup_call_cleanup(open_null_stream(Null),
			   (   copy_stream_data(Out, Null),
			       character_count(Null, Count)
			   ),
			   close(Null)).



		 /*******************************
		 *	   REGISTRATION		*
		 *******************************/

:- dynamic
	git_component/3,		% Name, Dir, Options
	git_component_version/2.	% Name, Version

%%	register_git_component(+Name, +Options)
%
%	Register the directory from which the  Prolog file was loaded as
%	a GIT component about which to  report version information. This
%	should be used as a directive.  Defined options:
%
%	    * directory(Dir)
%	    Use Dir as the location of the GIT repository instead of the
%	    directory of the file from which this directive was called.
%	    If Dir is not absolute, it is taken relative to the
%	    directory holding the file from which this directive was called.
%
%	    * home_url(URL)
%	    Used to create a link to the components home-page.

register_git_component(Name, Options) :-
	(   prolog_load_context(directory, BaseDir)
	->  true
	;   working_directory(BaseDir, BaseDir)
	),
	select_option(directory(Dir), Options, RestOptions, '.'),
	absolute_file_name(Dir, AbsDir,
			   [ file_type(directory),
			     relative_to(BaseDir),
			     access(read)
			   ]),
	retractall(git_component(Name, _, _)),
	assert(git_component(Name, AbsDir, RestOptions)),
	git_update_versions(Name).

git_update_versions(Name) :-
	catch(forall(git_component(Name, _, _),
		     update_version(Name)),
	      _,
	      print_message(warning, git(no_version))).

update_version(Name) :-
	git_component(Name, Dir, Options),
	git_describe(GitVersion, [directory(Dir)|Options]),
	retractall(git_component_version(Name, _)),
	assert(git_component_version(Name, GitVersion)).

%%	git_component_property(?Name, ?Property) is nondet.
%
%	Property is a property of the named git-component. Defined
%	properties are:
%
%	    * version(Version)
%	    git-describe like version information
%	    * directory(Dir)
%	    Base directory of the component
%
%	@tbd Extend with more detailed version (e.g., _remote_)

git_component_property(Name, Property) :-
	var(Name), !,
	git_component(Name, _, _),
	git_component_property(Name, Property).
git_component_property(Name, version(Version)) :-
	git_component_version(Name, Version).
git_component_property(Name, directory(Dir)) :-
	git_component(Name, Dir, _).
git_component_property(Name, Term) :-
	git_component(Name, _, Options),
	member(Term, Options).



		 /*******************************
		 *	  KEEP UP-TO-DATE	*
		 *******************************/

:- multifile
	user:message_hook/3.

user:message_hook(make(done(_)), _, _) :-
	git_update_versions(_),
	fail.

:- initialization
	git_update_versions(_).
