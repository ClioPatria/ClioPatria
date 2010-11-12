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

:- module(cpack,
	  [ cpack_discover/0,
	    cpack_package/2,		% +Name, -Resource
	    cpack_install/1,		% +Name
	    cpack_configure/1,		% +Name
	    cpack_add_dir/2,		% +ConfigEnabled, +Directory
	    cpack_create/3		% +Name, +Title, +Options
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(uri)).
:- use_module(library(lists)).
:- use_module(library(git)).
:- use_module(library(setup)).
:- use_module(library(conf_d)).
:- use_module(library(filesex)).
:- use_module(library(settings)).

/** <module> The ClioPatria package manager

*/

:- setting(cpack:package_directory, atom, cpack,
	   'Directory where packages are downloaded').

:- rdf_register_ns(cpack, 'http://www.swi-prolog.org/cliopatria/cpack#').

%%	cpack_discover is det.
%
%	Discover cpack packages.  Currently simply loads the library.

cpack_discover :-
	load_cpack_schema,
	forall(cpack_files(rdf(cpack), Files),
	       forall(member(Pack, Files),
		      rdf_load(Pack))).

%%	cpack_install(+Name) is det.
%
%	Install package by name

cpack_install(Name) :-
	cpack_discover,
	findall(Package, cpack_package(Name, Package), Packages),
	(   Packages == []
	->  existence_error(cpack, Name)
	;   Packages = [Package]
	->  cpack_install_package(Package)
	;   throw(error(ambiguity_error(Name, cpack, Packages),_))
	).


%%	cpack_configure(+Name) is det.
%
%	Just configure a package.

cpack_configure(Name) :-
	cpack_package_dir(Name, Dir, false),  !,
	exists_directory(Dir),
	(   conf_d_enabled(ConfigEnabled)
	->  cpack_add_dir(ConfigEnabled, Dir)
	;   existence_error(directory, 'config-enabled')
	).
cpack_configure(Name) :-
	existence_error(cpack, Name).


%%	cpack_package(+Name, -Package) is nondet.
%
%	True if Package is a ClioPatria package with Name.

cpack_package(Name, Package) :-
	rdf_has(Package, cpack:name, literal(Name)),
	rdfs_individual_of(Package, cpack:'Package').

%%	cpack_install(+Package) is det.
%
%	Install the package from its given URL

cpack_install_package(Package) :-
	cpack_install_dir(Package, Dir, false),
	cpack_download(Package, Dir),
	(   conf_d_enabled(ConfigEnabled)
	->  cpack_add_dir(ConfigEnabled, Dir)
	;   existence_error(directory, 'config-enabled')
	).

%%	cpack_download(Package, Dir)
%
%	Download and/or update Package to Dir.
%
%	@tbd	Branches, trust

cpack_download(_Package, Dir) :-
	directory_file_path(Dir, '.git', GitRepo),
	exists_directory(GitRepo), !,
	git([pull],
	    [ directory(Dir)
	    ]).				% Too simplistic
cpack_download(Package, Dir) :-
	rdf_has(Package, cpack:primaryRepository, Repo),
	rdf_has(Repo, cpack:repoURL, URL),
	git([clone, URL, Dir], []).

%%	cpack_add_dir(+ConfigEnable, +PackageDir)
%
%	Install package located in directory PackageDir.
%
%	@tbd	Register version-tracking with register_git_module/3.

cpack_add_dir(ConfigEnable, Dir) :-
	directory_file_path(ConfigEnable, '010-packs.pl', PacksFile),
	directory_file_path(Dir, 'config-available', ConfigAvailable),
	file_base_name(Dir, Pack),
	add_pack_to_search_path(PacksFile, Pack, Dir),
	setup_default_config(ConfigEnable, ConfigAvailable, []),
	conf_d_reload.


add_pack_to_search_path(PackFile, Pack, Dir) :-
	exists_file(PackFile), !,
	read_file_to_terms(PackFile, Terms, []),
	(   memberchk(user:file_search_path(Pack, Dir), Terms)
	->  true
	;   memberchk(user:file_search_path(Pack, _Dir2), Terms),
	    permission_error(add, pack, Pack)
	;   open(PackFile, append, Out),
	    extend_search_path(Out, Pack, Dir),
	    close(Out)
	).
add_pack_to_search_path(PackFile, Pack, Dir) :-
	open(PackFile, write, Out),
	format(Out, '/* Generated file~n', []),
	format(Out, '   This file defines the search-path for added packs~n', []),
	format(Out, '*/~n~n', []),
	format(Out, ':- module(conf_packs, []).~n~n', []),
	format(Out, ':- multifile user:file_search_path/2.~n', []),
	format(Out, ':- dynamic user:file_search_path/2.~n~n', []),
	extend_search_path(Out, Pack, Dir),
	close(Out).

extend_search_path(Out, Pack, Dir) :-
	Term =.. [Pack, '.'],
	format(Out, '~q.~n', [user:file_search_path(Pack, Dir)]),
	format(Out, '~q.~n', [user:file_search_path(cpacks, Term)]).


		 /*******************************
		 *	CREATE NEW PACKAGES	*
		 *******************************/

%%	cpack_create(+Name, +Title, +Options) is det.
%
%	Create a new package.  Options include
%
%	  * type(Type)
%	  Label of a subclass of cpack:Package.  Default is =package=
%	  * title(Title)
%	  Title for the package.  Should be a short line.
%	  * foafname(FoafName)
%	  foaf:name to put into the default template
%	  * foafmbox(Email)
%	  foaf:mbox to put into the default template
%
%	Default options are  extracted  from   the  cpack:Profile  named
%	=default=
%
%	@tbd	Allow selection profile, auto-loading of profile, etc.

cpack_create(Name, Title, Options) :-
	option(type(Type), Options, package),
	package_class_id(Type, PkgClass),
	default_bindings(default, Name, DefaultBindings),
	merge_options(Options,
		      [ name(Name),
			title(Title),
			pkgclass(PkgClass)
		      | DefaultBindings
		      ], Vars),
	cpack_package_dir(Name, Dir, true),
	forall(cpack_dir(SubDir, Type),
	       make_cpack_dir(Dir, SubDir)),
	forall(cpack_template(In, Out),
	       install_template_file(In, Out, Vars)),
	git([init], [directory(Dir)]),
	git([add, '.'], [directory(Dir)]),
	git([commit, '-m', 'Installed template'], [directory(Dir)]),
	git([tag, epoch], [directory(Dir)]),
	git_setup_push(Dir, Vars).

package_class_id(Label, TurtleID) :-
	package_class(Label, Class),
	rdf_global_id(Prefix:Name, Class),
	atomic_list_concat([Prefix, :, Name], TurtleID).

package_class(Label, Class) :-
	rdf_has(Class, rdfs:label, literal(Label)),
	rdfs_subclass_of(Class, cpack:'Package'), !.
package_class(Label, _) :-
	domain_error(package_class, Label).

default_bindings(Profile, Name, Bindings) :-
	findall(B, default_binding(Profile, Name, B), Bindings).

default_binding(ProfileName, Name, B) :-
	rdf_has(Profile, cpack:name, literal(ProfileName)),
	(   rdf_has(Profile, cpack:defaultAuthor, Author),
	    (   rdf_has(Author, foaf:name, literal(AuthorName)),
		B = foafname(AuthorName)
	    ;   rdf_has(Author, foaf:mbox, literal(MBOX)),
		B = foafmbox(MBOX)
	    )
	;   rdf_has(Profile, cpack:fetchRepositoryTemplate, literal(GitTempl)),
	    substitute(GitTempl, '@CPACK@', Name, GitRepo),
	    B = fetchrepository(GitRepo)
	;   rdf_has(Profile, cpack:pushRepositoryTemplate, literal(GitTempl)),
	    substitute(GitTempl, '@CPACK@', Name, GitRepo),
	    B = pushrepository(GitRepo)
	).

%%	git_setup_push(+Dir, +Vars) is det.
%
%	Set an origin for the newly  created repository. This also tries
%	to  setup  a  bare  repository  at   the  remote  machine  using
%	git_create_origin/2.

git_setup_push(Dir, Vars) :-
	option(pushrepository(PushURL), Vars), !,
	option(title(Title), Vars, 'ClioPatria CPACK'),
	git([remote, add, origin, PushURL], [directory(Dir)]),
	directory_file_path(Dir, '.git/config', Config),
	setup_call_cleanup(open(Config, append, Out),
			   format(Out, '[branch "master"]\n\
			   		\tremote = origin\n\
					\tmerge = refs/heads/master\n', []),
			   close(Out)),
	catch(git_create_origin(PushURL, Title), E,
	      print_message(error, E)).
git_setup_push(_,_).

%%	git_create_origin(+PushURL, +Title) is det.
%
%	Try to create the repository origin. As the user has setup push,
%	we hope he setup SSH appropriately. Note that this only works if
%	the remote user has a real shell and not a git-shell.

git_create_origin(PushURL, Title) :-
	uri_components(PushURL, Components),
	uri_data(scheme, Components, Scheme),
	(   Scheme == ssh
	->  uri_data(authority, Components, Authority)
	;   Authority = Scheme
	),
	uri_data(path, Components, Path),
	file_directory_name(Path, Parent),
	file_base_name(Path, Repo),
	format(atom(Command),
	       'cd "~w" && mkdir "~w" && cd "~w" && \
	       git init --bare && echo "~w" > description && \
	       touch git-daemon-export-ok',
	       [Parent, Repo, Repo, Title]),
	process_create(path(ssh), [ Authority, Command ], []).


%%	make_cpack_dir(+BaseDir, +CPACKDir) is det.
%
%	Setup th directory structure for a new package.

make_cpack_dir(Dir, SubDir) :-
	directory_file_path(Dir, SubDir, New),
	(   exists_directory(New)
	->  true
	;   make_directory_path(New),
	    print_message(informational, cpack(create_directory(New)))
	).

install_template_file(In, Out, Vars) :-
	option(name(Name), Vars),
	absolute_file_name(In, InFile, [access(read)]),
	substitute(Out, '@NAME@', Name, OutFile),
	cpack_package_dir(Name, Dir, true),
	directory_file_path(Dir, OutFile, OutPath),
	copy_file_with_vars(InFile, OutPath, Vars),
	print_message(informational, cpack(installed_template(OutFile))).

substitute(In, From, To, Out) :-
	sub_atom(In, B, _, A, From), !,
	sub_atom(In, 0, B, _, Start),
	sub_atom(In, _, A, 0, End),
	atomic_list_concat([Start, To, End], Out).
substitute(In, _, _, In).

cpack_dir(rdf, _).
cpack_dir('rdf/cpack', _).
cpack_dir('config-available', _).
cpack_dir('applications', _).
cpack_dir('components', _).
cpack_dir('lib', _).
cpack_dir('web', _).
cpack_dir('web/js', _).
cpack_dir('web/css', _).
cpack_dir('web/html', _).

cpack_template(library('cpack/config-available.pl.in'),
	       'config-available/@NAME@.pl').
cpack_template(library('cpack/DEFAULTS.in'),
	       'config-available/DEFAULTS').
cpack_template(library('cpack/pack.ttl.in'),
	       'rdf/cpack/@NAME@.ttl').


		 /*******************************
		 *	       UTIL		*
		 *******************************/

load_cpack_schema :-
	rdf_load(rdf('tool/cpack.ttl')).

cpack_files(Spec, Files) :-
	absolute_file_name(Spec, Dir,
			   [ file_type(directory),
			     solutions(all)
			   ]),
	directory_file_path(Dir, '*.*', Pattern), % must have some extension
	expand_file_name(Pattern, AllFiles),
	include(rdf_file, AllFiles, Files).

rdf_file(File) :-
	file_name_extension(_, Ext, File),
	rdf_extension(Ext).

rdf_extension(rdf).
rdf_extension(ttl).

%%	cpack_install_dir(+Package, -Dir, +Create)
%
%	Installation directory for Package

cpack_install_dir(Package, Dir, Create) :-
	rdf_has(Package, cpack:name, literal(Name)),
	cpack_package_dir(Name, Dir, Create).

cpack_package_dir(Name, Dir, Create) :-
	setting(cpack:package_directory, PackageDir),
	directory_file_path(PackageDir, Name, Dir),
	(   (   Create == false
	    ;	exists_directory(Dir)
	    )
	->  true
	;   make_directory(Dir)
	).

:- multifile prolog:message//1.

prolog:message(cpack(create_directory(New))) -->
	[ 'Created directory ~w'-[New] ].
prolog:message(cpack(installed_template(File))) -->
	[ 'Installed template ~w'-[File] ].

:- if(\+current_predicate(directory_file_path/3)).

directory_file_path(Dir, File, Path) :-
	(   sub_atom(Dir, _, _, 0, /)
	->  atom_concat(Dir, File, Path)
	;   atomic_list_concat([Dir, /, File], Path)
	).

:- endif.
