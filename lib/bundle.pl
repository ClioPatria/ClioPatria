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

:- module(bundle,
	  [ use_bundle/1
	  ]).

/** <module> Import bundles

This module provides an interface that is   similar  to the Ciao package
interface. Bundles are Prolog files that   are included. Typically, they
contain multiple use_module/1 calls.

At least for now, the system  has   been  named  bundle because the name
package is already used as a package of software.
*/

:- multifile
	user:file_search_path/2,
	system:term_expansion/2,
	emacs_prolog_colours:goal_colours/2.

user:file_search_path(bundle,	    library(bundles)).

%%	use_bundle(+Term)
%
%	Include a bundle of declarations.  Typically, these declarations
%	are a set of :- use_module(Library). calls.

use_bundle(Package) :-
	throw(error(context_error(nodirective, use_bundle(Package)), _)).

system:term_expansion((:- use_bundle(Package)),
		      (:- include(bundle(Package)))).


emacs_prolog_colours:goal_colours(use_bundle(Pkg),
				  built_in - [ Class ]) :-
	(   absolute_file_name(bundle(Pkg), File,
			       [ file_type(prolog),
				 file_errors(fail)
			       ])
	->  Class = file(File)
	;   Class = nofile
	).
