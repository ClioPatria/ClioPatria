/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2010, University of Amsterdam
			      VU University Amsterdam

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

:- module(rdf_turtle_io,
	  [
	  ]).
:- use_module(library(semweb/rdf_turtle_write)).

:- multifile
	rdf_io:write_graph/4.

/** <module> Write query-result graphs as Turtle

This module writes a SPARQL graph results   using  the Turtle format. It
acts as a hook into rdf_io.pl.
*/

		 /*******************************
		 *	    GRAPH OUTPUT	*
		 *******************************/

%%	rdf_io:write_graph(+Format, +Serialization, +Triples, +Options)
%
%	Write an RDF result-graph as an  HTML table, where resources are
%	links to the ClioPatria local view.
%
%	@param Format is one of =turtle= or =canonical_turtle=
%	@param Serialization is ignored
%	@param Triples is a list of rdf(S,P,O) triples

rdf_io:write_graph(turtle, _Serialization, Triples, Options) :-
	option(mimetype(Type), Options, 'text/turtle'),
	format('Transfer-encoding: chunked~n'),
	format('Content-type: Type~n~n', [Type]),
	(   Triples == []
	->  format('# Graph contains no data~n', [])
	;   rdf_save_turtle(stream(current_output),
			    [ expand(triple_in(Triples))
			    ])
	).
rdf_io:write_graph(canonical_turtle, _Serialization, Triples, Options) :-
	option(mimetype(Type), Options, 'text/turtle'),
	format('Transfer-encoding: chunked~n'),
	format('Content-type: Type~n~n', [Type]),
	(   Triples == []
	->  format('# Graph contains no data~n', [])
	;   rdf_save_canonical_turtle(stream(current_output),
				      [ expand(triple_in(Triples))
				      ])
	).

:- public
	triple_in/5.

triple_in(RDF, S,P,O,_G) :-
	member(rdf(S,P,O), RDF).
