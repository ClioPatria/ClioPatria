/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2006, University of Amsterdam

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

:- module(rdf_store,
	  [ rdf_setup_store/0,
	    rdf_setup_store/1,		% +Options
	    rdf_init_db/1		% +Type
	  ]).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdf_persistency')).
:- use_module(library(settings)).
:- use_module(parms).

%%      rdf_setup_store is det.
%%      rdf_setup_store(+Options) is det.
%
%	Initialise persistent storage. If the database is empty, load
%	rdfs.rdfs into it.  Options are passed to rdf_attach_db/2.
%	
%	@tbd	Also process options to deal with empty database

rdf_setup_store :-
	rdf_setup_store([]).

rdf_setup_store(Options) :-
	setting(serql_parms:persistent_store, Directory),
	Directory \== '', !,
        (   exists_directory(Directory)
        ->  rdf_attach_db(Directory, Options)
        ;   rdf_attach_db(Directory, Options),
	    setting(serql_parms:base_ontologies, Base),
            rdf_init_db(Base)
        ).
rdf_setup_store(_) :-
	setting(serql_parms:base_ontologies, Base), !,
	rdf_init_db(Base).
rdf_setup_store(_) :-
        rdf_init_db(serql(rdfs)).


%%      rdf_init_db(+Base) is det.
%
%	Clears the repository and loads Base.  Note that this call cannot
%	be embedded in a transaction!

rdf_init_db(Base) :-
        rdf_reset_db,
	rdf_load(Base).

