/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2004, University of Amsterdam

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

:- module(rdf_io,
	  [ write_table/2,	% +Row, +Options
	    write_graph/2,	% +Triples, +Options
	    load_triples/2,	% +Input, +Options
	    get_triples/3	% +Input, -Triples, +Options
	  ]).
:- use_module(library('semweb/rdf_db')).
:- use_module(library(rdf_write)).
:- use_module(library(rdf)).
:- use_module(library(lists)).

:- multifile
	write_table/4,		% +Format, +Serialization, +Rows, +Options
	write_graph/4,		% +Format, +Serialization, +Triples, +Options
	load_triples/3,		% +Format, +Input, +Options
	get_triples/4.		% +Format, +Input, -Triples, +Options

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module acts as a dispatcher module,   allowing other modules to add
clauses  for  write_table/4  and  write_graph/4    and   thus  providing
additional output formats without modifications to the kernel source.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	write_table(+Rows, +Options)
%	
%	Write a result-table in the specified format.  Rows is a list of
%	terms row(C1, C2, ...).  Options specifies additional processing
%	options.  Defined options are:
%	
%		* variables(+Vars)
%		Specifies the names of the columns.  Vars is a term with
%		functor =vars= and atom-arguments describing the names
%		of each subsequent column.  For Example:
%		
%		==
%		variables(vars('Name', 'Address'))
%		==
		

write_table(Rows, Options) :-
	needed_option(result_format(Format), Options),
	needed_option(serialization(Serialization), Options),
	write_table(Format, Serialization, Rows, Options).


write_graph(Triples, Options) :-
	needed_option(serialization(Serialization), Options),
	(   Serialization == rdfxml
	->  (   memberchk(result_format(Format), Options)
	    ->	true
	    ;	Format = xml
	    )
	;   needed_option(result_format(Format), Options)
	),
	write_graph(Format, Serialization, Triples, Options).


		 /*******************************
		 *	       READING		*
		 *******************************/

load_triples(Input, Options0) :-
	select(data_format(Format), Options0, Options),
	load_triples(Format, Input, Options).


load_triples(rdfxml, Input, Options) :- !,
	rdf_load(Input, Options).

get_triples(Input, Triples, Options0) :-
	select(data_format(Format), Options0, Options),
	get_triples(Format, Input, Triples, Options).


get_triples(rdfxml, Input, Triples, Options) :- !,
	load_rdf(Input, Triples, Options).


		 /*******************************
		 *	       HOOK		*
		 *******************************/

%%	write_graph(+ResultFormat, +Serialization, +Triples, +Options)
%	
%	Provide hook for rdf_io.pl plugin interface

write_graph(xml, rdfxml, Triples, _Options) :-
	format('Content-type: application/rdf+xml~n~n'),
	rdf_write_xml(current_output, Triples).

		 /*******************************
		 *		UTIL		*
		 *******************************/

needed_option(Term, Options) :-
	memberchk(Term, Options), !.
needed_option(Term, _) :-
	functor(Term, Name, _),
	throw(error(existence_error(option, Name), _)).
