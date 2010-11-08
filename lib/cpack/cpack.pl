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
	    cpack_create/2		% +Name, +Options
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(uri)).
:- use_module(library(lists)).
:- use_module(library(git)).
:- use_module(library(setup)).
:- use_module(library(conf_d)).
:- use_module(library(filesex)).

/** <module> The ClioPatria package manager

*/

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
	cpack_install_dir(Name, Dir, false),  !,
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

%%	cpack_create(+Name, +Options) is det.
%
%	Create a new package.

cpack_create(Name, Options) :-
	option(type(Type), Options, _),
	cpack_package_dir(Name, Dir, true),
	forall(cpack_dir(SubDir, Type),
	       make_cpack_dir(Dir, SubDir)),
	git([init], [directory(Dir)]).

make_cpack_dir(Dir, SubDir) :-
	directory_file_path(Dir, SubDir, New),
	make_directory_path(New),
	print_message(informational, cpack(create_directory(New))).

cpack_dir(rdf, _).
cpack_dir('rdf/cpack', _).
cpack_dir('applications', code).
cpack_dir('components', code).
cpack_dir('lib', code).
cpack_dir('web', web).
cpack_dir('web/js', web).
cpack_dir('web/css', web).
cpack_dir('web/html', web).


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
	directory_file_path('cpack', Name, Dir),
	(   (   Create == false
	    ;	exists_directory(Dir)
	    )
	->  true
	;   make_directory(Dir)
	).

:- multifile prolog:message//1.

prolog:message(cpack(create_directory(New))) -->
	[ 'Created directory ~w'-[New] ].

:- if(\+current_predicate(directory_file_path/3)).

directory_file_path(Dir, File, Path) :-
	(   sub_atom(Dir, _, _, 0, /)
	->  atom_concat(Dir, File, Path)
	;   atomic_list_concat([Dir, /, File], Path)
	).

:- endif.
