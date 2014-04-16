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
	    register_git_module/2,	% +Name, +Options
	    git_module_property/2,	% ?Name, ?Property
	    git_update_versions/1	% ?Name
	  ]).
:- use_module(library(process)).
:- use_module(library(option)).
:- use_module(library(readutil)).
:- use_module(library(git)).


/** <module> Manage software versions

The module deals with software  versions.   It  currently implements two
features:  test  whether   SWI-Prolog   is    sufficiently   new   using
check_prolog_version/1 and find GIT version   signatures for the running
server. Modules that want  their  version   info  available  through the
web-page can do so using a call to register_git_module/2.
*/

:- multifile
	git_module_hook/3.		% Name, Dir, Options

%%	check_prolog_version(+Required)
%
%	Validate the program is running under Prolog version Required or
%	newer. Required is in numeric notation (e.g. 50317 for 5.3.17)

check_prolog_version(Required) :-
	prolog_version_ok(Required), !.
check_prolog_version(Required) :-
	print_message(error,
		      required_prolog_version(Required)),
	format(user_error, '~nPress any key to exit> ', []),
	get_single_char(_), nl(user_error),
	halt(1).

prolog_version_ok(or(V1, V2)) :-
	(   prolog_version_ok(V1)
	->  true
	;   prolog_version_ok(V2)
	).
prolog_version_ok(Required) :-
        current_prolog_flag(version, MyVersion),
	MyVersion >= Required.

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
prolog:message(git(update_versions)) -->
	[ 'Updating GIT version stamps in the background.' ].


user_version(or(V1,V2), Version) :- !,
	user_version(V1, A1),
	user_version(V2, A2),
	format(atom(Version), '~w or ~w', [A1, A2]).
user_version(N, Version) :-
        Major is N // 10000,
        Minor is (N // 100) mod 100,
        Patch is N mod 100,
        atomic_list_concat([Major, Minor, Patch], '.', Version).


		 /*******************************
		 *	   REGISTRATION		*
		 *******************************/

:- dynamic
	git_module/3,		% Name, Dir, Options
	git_module_version/2.	% Name, Version

%%	register_git_module(+Name, +Options)
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

register_git_module(Name, Options) :-
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
	retractall(git_module(Name, _, _)),
	assert(git_module(Name, AbsDir, RestOptions)).

git_update_versions(Name) :-
	catch(forall(current_git_module(Name, _, _),
		     update_version(Name)),
	      _,
	      print_message(warning, git(no_version))).

update_version(Name) :-
	current_git_module(Name, Dir, Options),
	(   catch(git_describe(GitVersion, [directory(Dir)|Options]), _, fail)
	->  true
	;   GitVersion = unknown
	),
	retractall(git_module_version(Name, _)),
	assert(git_module_version(Name, GitVersion)).

current_git_module(Name, Dir, Options) :-
	git_module(Name, Dir, Options).
current_git_module(Name, Dir, Options) :-
	git_module_hook(Name, Dir, Options).


%%	git_module_property(?Name, ?Property) is nondet.
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

git_module_property(Name, Property) :-
	(   var(Name)
	->  current_git_module(Name, _, _),
	    git_module_property(Name, Property)
	;   compound(Property)
	->  once(gen_module_property(Name, Property))
	;   gen_module_property(Name, Property)
	).

gen_module_property(Name, version(Version)) :-
	(   git_module_version(Name, Version0)
	->  true
	;   git_update_versions(Name),
	    git_module_version(Name, Version0)
	),
	Version0 \== unknown,
	Version = Version0.
gen_module_property(Name, directory(Dir)) :-
	current_git_module(Name, Dir, _).
gen_module_property(Name, Term) :-
	current_git_module(Name, _, Options),
	member(Term, Options).



		 /*******************************
		 *	  KEEP UP-TO-DATE	*
		 *******************************/

bg_git_update_versions :-
	print_message(informational, git(update_versions)),
	thread_create(git_update_versions(_), _,
		      [ detached(true)
		      ]).

:- multifile
	user:message_hook/3.

user:message_hook(make(done(_)), _, _) :-
	bg_git_update_versions,
	fail.

% do not update versions in background because we need to fork
:- if(current_predicate(http_unix_daemon:http_daemon/0)).
:- initialization git_update_versions(_).
:- else.
:- initialization bg_git_update_versions.
:- endif.
