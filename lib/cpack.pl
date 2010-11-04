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
	  [ install_package_dir/1	% +Directory
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

/** <module> The ClioPatria package manager



*/

:- rdf_register_ns(cpack, 'http://www.swi-prolog.org/cliopatria/cpack#').

%%	install_package_dir(+PackageDir)
%
%	Install package located in directory PackageDir.

install_package_dir(Dir) :-
	load_cpack_schema,
	absolute_file_name(Dir, Path,
			   [ file_type(directory),
			     access(read)
			   ]),
	directory_file_path(Path, 'Pack.ttl', PackFile),
	rdf_load(PackFile, [graph(Graph)]),
	rdf_has(Graph, cpack:name, literal(Pack)),
	assert(user:file_search_path(Pack, Path)),
	DirAlias =.. [Pack,'.'],
	assert(user:file_search_path(cpack, DirAlias)).


load_cpack_schema :-
	rdf_load(ontology('tool/cpack.ttl')).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

directory_file_path(Dir, File, Path) :-
	(   sub_atom(Dir, _, _, 0, /)
	->  atom_concat(Dir, File, Path)
	;   atomic_list_concat([Dir, /, File], Path)
	).
