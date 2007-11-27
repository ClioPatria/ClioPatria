/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(rdf_portray,
	  [ rdf_portray_as/1		% +Style
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(error)).

:- dynamic
	style/1.

rdf_portray_as(Style) :-
	must_be(oneof([write, ns:id, ns:label]), Style),
	retractall(style(_)),
	assert(style(Style)).

:- multifile
	user:portray/1.

user:portray(URL) :-
	atom(URL),
	sub_atom(URL, 0, _, _, 'http://'),
	(   style(write)
	->  write(URL)
	;   style(ns:id)
	->  (   rdf_global_id(NS:Id, URL)
	    ->	writeq(NS:Id)
	    ;	writeq(URL)
	    )
	;   rdfs_ns_label(URL, Label),
	    write(Label)
	).
user:portray(URL) :-
	atom(URL),
	atom_concat('__file://', URL2, URL),
	sub_atom(URL2, S, _, A, #),
	sub_atom(URL2, _, A, 0, Local),
	sub_atom(URL2, 0, S, _, Path),
	file_base_name(Path, Base),
	format('__~w#~w', [Base, Local]).
