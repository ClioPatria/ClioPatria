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
	    cpack_add_dir/1		% +Directory
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(uri)).
:- use_module(library(git)).

/** <module> The ClioPatria package manager

*/

:- rdf_register_ns(cpack, 'http://www.swi-prolog.org/cliopatria/cpack#').

%%	cpack_discover is det.
%
%	Discover cpack packages.  Currently simply loads the library.

cpack_discover :-
	load_cpack_schema,
	cpack_files(rdf(cpack), Files),
	forall(member(Pack, Files),
	       rdf_load(Pack, [format(turtle)])).

%%	cpack_package(+Name, -Package) is nondet.
%
%	True if Package is a ClioPatria package with Name.

cpack_package(Name, Package) :-
	rdf_has(Package, cpack:name, literal(Name)),
	rdfs_individual_of(Package, cpack:'Package').

%%	cpack_install(+Package) is det.
%
%	Install the package from its given URL

cpack_install(Package) :-
	cpack_install_dir(Package, Dir),
	cpack_download(Package, Dir),
	cpack_add_dir(Dir).

%%	cpack_download(Package, Dir)
%
%	Download and/or update Package to Dir.
%
%	@tbd	Branches, trust

cpack_download(_Package, Dir) :-
	exists_directory(Dir), !,
	git([pull],
	    [ directory(Dir)
	    ]).				% Too simplistic
cpack_download(Package, Dir) :-
	rdf_has(Package, cpack:primaryRepository, Repo),
	rdf_has(Repo, cpack:repoURL, URL),
	git([clone, URL, Dir], []).

%%	cpack_add_dir(+PackageDir)
%
%	Install package located in directory PackageDir.

cpack_add_dir(Dir) :-
	load_cpack_schema,
	absolute_file_name(Dir, Path,
			   [ file_type(directory),
			     access(read)
			   ]),
	cpack_file(Path, File),
	rdf_load(File, [graph(Graph), format(turtle)]),
	rdf_has(Graph, cpack:name, literal(Pack)),
	assert(user:file_search_path(Pack, Path)),
	DirAlias =.. [Pack,'.'],
	assert(user:file_search_path(cpacks, DirAlias)).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

load_cpack_schema :-
	rdf_load(rdf('tool/cpack.ttl')).

%%	cpack_file(+Dir, -File) is det.
%
%	File is the pack information file for a package in Dir.

cpack_file(Dir, File) :-
	cpack_files(Dir, Files),
	(   Files = [File]
	->  true
	;   File == []
	->  existence_error(pack_file, Dir)
	;   throw(error(ambiguity_error(pack_file, Dir)))
	).

cpack_files(Dir, Files) :-
	directory_file_path(Dir, '*.cpack', Pattern),
	expand_file_name(Pattern, Files).

%%	cpack_install_dir(+Package, -Dir)
%
%	Installation directory for Package

cpack_install_dir(Package, Dir) :-
	rdf_has(Package, cpack:name, literal(Name)),
	directory_file_path('cpack', Name, Dir).

%%	directory_file_path(+Directory, +File, -Path) is det.

directory_file_path(Dir, File, Path) :-
	(   compound(Dir)
	->  absolute_file_name(Dir, TheDir,
			       [ file_type(directory),
				 access(read)
			       ])
	;   TheDir = Dir
	),
	(   sub_atom(TheDir, _, _, 0, /)
	->  atom_concat(TheDir, File, Path)
	;   atomic_list_concat([TheDir, /, File], Path)
	).
