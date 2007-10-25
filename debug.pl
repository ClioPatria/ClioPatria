/*  This file is part of ClioPatria.

    Author:
    HTTP:	http://e-culture.multimedian.nl/
    GITWEB:	http://gollem.science.uva.nl/git/ClioPatria.git
    GIT:	git://gollem.science.uva.nl/home/git/ClioPatria.git
    GIT:	http://gollem.science.uva.nl/home/git/ClioPatria.git
    Copyright:  2007, E-Culture/MultimediaN

    ClioPatria is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    ClioPatria is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ClioPatria.  If not, see <http://www.gnu.org/licenses/>.
*/

/*  File:    debug.pl
    Author:  Jan Wielemaker
    Created: Aug  2 2004
    Purpose: 
*/

:- module(rdf_debug,
	  [ count/1
	  ]).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).


		 /*******************************
		 *	       MEASURE		*
		 *******************************/

:- meta_predicate
	count(:).

count(G) :-
	get_time(T0),
	statistics(cputime, CPU0),
	C = c(0),
	(   G,
	    arg(1, C, C0),
	    C1 is C0+1,
	    nb_setarg(1, C, C1),
	    fail
	;   arg(1, C, Count)
	),
	statistics(cputime, CPU1),
	get_time(T1),
	CPU is CPU1 - CPU0,
	Wall is T1 - T0,
	format('~D solutions, ~2f CPU in ~2f seconds~n',
	       [ Count, CPU, Wall ]).


		 /*******************************
		 *	     PORTRAY		*
		 *******************************/

:- multifile
	user:portray/1.

user:portray(X) :-
	atom(X),
	rdf_global_id(NS:Local, X),
	(   rdfs_label(X, Label)
	->  format('~q (~w)', [NS:Local, Label])
	;   writeq(NS:Local)
	).
