/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2011, University of Amsterdam,
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

:- module(rdf_file_type,
	  [ rdf_guess_data_format/2,	% +Stream, ?Format
	    rdf_guess_format_and_load/2	% +Stream, +Options
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(sgml)).

/** <module> Load RDF data from unknown file-type

*/


%%	rdf_guess_format_and_load(+Stream, +Options) is det.
%
%	Guess the RDF format in Stream  and   load  it. Stream must be a
%	_repositional_ stream.

rdf_guess_format_and_load(Stream, Options) :-
	option(format(_), Options), !,
	rdf_load(stream(Stream), Options).
rdf_guess_format_and_load(Stream, Options) :-
	rdf_guess_data_format(Stream, Format),
	rdf_load(stream(Stream), [format(Format)|Options]).

%%	rdf_guess_data_format(+Stream, ?Format)
%
%	Guess the format  of  an  RDF   file  from  the  actual content.
%	Currently, this seeks for a valid  XML document upto the rdf:RDF
%	element before concluding that the file is RDF/XML. Otherwise it
%	assumes that the document is Turtle.

rdf_guess_data_format(_, Format) :-
	nonvar(Format), !.
rdf_guess_data_format(Stream, xml) :-
	xml_doctype(Stream, _), !.
rdf_guess_data_format(_, turtle).


%%	xml_doctype(+Stream, -DocType) is semidet.
%
%	Parse a _repositional_ stream and get the  name of the first XML
%	element *and* demand that this   element defines XML namespaces.
%	Fails if the document is illegal XML before the first element.
%
%	Note that it is not  possible   to  define valid RDF/XML without
%	namespaces, while it is not possible  to define a valid absolute
%	Turtle URI (using <URI>) with a valid xmlns declaration.

xml_doctype(Stream, DocType) :-
	catch(setup_call_cleanup(make_parser(Stream, Parser, State),
				 sgml_parse(Parser,
					    [ source(Stream),
					      max_errors(1),
					      syntax_errors(quiet),
					      call(begin, on_begin),
					      call(cdata, on_cdata)
					    ]),
				 cleanup_parser(Stream, Parser, State)),
	      E, true),
	nonvar(E),
	E = tag(DocType).

make_parser(Stream, Parser, state(Pos)) :-
	stream_property(Stream, position(Pos)),
	new_sgml_parser(Parser, []),
	set_sgml_parser(Parser, dialect(xmlns)).

cleanup_parser(Stream, Parser, state(Pos)) :-
	free_sgml_parser(Parser),
	set_stream_position(Stream, Pos).

on_begin(Tag, Attributes, _Parser) :-
	memberchk(xmlns:_=_, Attributes),
	throw(tag(Tag)).

on_cdata(_CDATA, _Parser) :-
	throw(error(cdata)).
