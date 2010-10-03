/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010 VU University Amsterdam

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

:- module(conf_d,
	  [ load_conf_d/2		% +Directory, +Options
	  ]).
:- use_module(library(option)).
:- use_module(library(apply)).

/** <module> Load configuration directories

This module deals with  loading   configuration-files  from a directory.
This is pretty simple because  we   assume  that configuration files are
Prolog  source-files.  We  (can)  use    file_search_path/2   to  define
one or more configuration directories.

Files are loaded in alphabetical  order.   If  one  config file requires
another, there are two solutions:

    * Use some numbering scheme, e.g., name the files 00-prefixes.pl,
    01-paths.pl, etc.
    * Use a use_module/1 call to include the config file(s) on which we
    depend.
*/

%%	load_conf_d(+Spec, +Options) is det.
%
%	Locate configuration directories and load   their  config files.
%	Config files themselves are Prolog source files.  Options:
%
%	    * solutions(+Sols)
%	    Passed to absolute_file_name/3.  Default is =all=, loading
%	    config files from all directories described by Spec.
%	    * extension(+Ext)
%	    File-name extension for the config files.  Default is =pl=.
%
%	Other options are passed to load_files/2.

load_conf_d(Spec, Options) :-
	select_option(solutions(Sols), Options, LoadOptions0, all),
	merge_options(LoadOptions0,
		      [ if(changed),
			extension(pl)
		      ], LoadOptions),
	forall(absolute_file_name(Spec, Dir,
				  [ file_type(directory),
				    file_errors(fail),
				    access(read),
				    solutions(Sols)
				  ]),
	       load_conf_dir(Dir, LoadOptions)).

load_conf_dir(Dir, Options) :-
	select_option(extension(Ext), Options, LoadOptions),
	atomic_list_concat([Dir, '/*.', Ext], Pattern),
	expand_file_name(Pattern, Files),
	include(accessible, Files, AccessibleFiles),
	sort(AccessibleFiles, Sorted),
	load_files(user:Sorted, LoadOptions).

accessible(File) :-
	access_file(File, read).
