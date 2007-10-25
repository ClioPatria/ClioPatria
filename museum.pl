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

:- module(serql_museum,
	  [ login/0,			% Login to server
	    local/0,			% Use local server
	    sesame/0,			% Use Sesame java server
	    upload/0,			% Upload museum data
	    query/2,			% +QID, -Result

	    all_ok/0,
	    ok/1,
	    all_val/0,
	    val/1,
	    all_run/0,
	    run/1
	  ]).

:- load_files([ sesame_client,
		test_client
	      ],
	      [ silent(true)
	      ]).

:- style_check(-atom).

user:file_search_path(sesame, '/staff/jan/src/openrdf').
user:file_search_path(sesame_museum, sesame('htdocs/files/museum')).


query(N, Result) :-
	q(N, Query, English),
	format('Query: ~w~n', [English]),
	run_query(Query, Result).

run_query(Query, Result) :- 
	(   atom_concat('select', _, Query)
	->  sesame_table_query(Query, Result, [])
	;   sesame_graph_query(Query, Result, [])
	).

upload :-
	clear,
	upload(sesame_museum(culture), []),
	upload(sesame_museum(schema1),
	       [ base_uri('http://www.icom.com/schema.rdf')
	       ]),
	upload(sesame_museum(schema2),
	       [ base_uri('http://www.oclc.org/schema.rdf')
	       ]).


ok_file(Q, File) :-
	Dir = 'museum',
	(   exists_directory(Dir)
	->  true
	;   make_directory(Dir)
	),
	concat_atom([Dir, /, Q, '.ok'], File).

all_ok :-
	forall(q(Q, _, _),
	       ok(Q)).

ok(Q) :-
	q(Q, Query, _),
	(   setof(R, run_query(Query, R), Set)
	->  true
	;   Set = []
	),
	ok_file(Q, File),
	open(File, write, Out),
	forall(member(T, Set),
	       format(Out, '~q.~n', [T])),
	close(Out).

all_val :-
	forall(q(Q, _, _),
	       val(Q)).

val(Q) :-
	q(Q, Query, _),
	(   setof(R, run_query(Query, R), Set)
	->  true
	;   Set = []
	),

	ok_file(Q, File),
	read_file_to_terms(File, OK, []),

	(   OK =@= Set
	->  true
	;   format('ERROR: Query ~w~n', [Q]),
	    report_diff(OK, Set)
	).

all_run :-
	forall(q(Q, _, _),
	       run(Q)).

run(Q) :-
	q(Q, Query, _),
	forall(run_query(Query, _Result), true).

report_diff(OK, Result) :-
	subtract(OK, Result, Missing),
	subtract(Result, OK, TooMany),
	(   Missing \== []
	->  format('**************** Omitted results:~n'),
	    write_list(Missing)
	;   true
	),
	(   TooMany \== []
	->  format('**************** Overcomplete results:~n'),
	    write_list(TooMany)
	;   true
	).

write_list([]).
write_list([H|T]) :-
	format('~p~n', [H]),
	write_list(T).


%%	q(?N, ?Query, ?Description)
%	
%	Example queries from
%	
%		http://www.openrdf.org/sesame/serql/serql-examples.html

q(1, 'select *
      from   {X} p {Y}',
  'Retrieve all statements').
q(2, 'select *
     from   {<!http://www.european-history.com/picasso.html>} p {Y}',
  'Retrieve all properties and their values about Picasso').
q(3, 'select Painter, Painting, Technique
      from   {Painter} <rdf:type> {<cult:Painter>};
		       <cult:paints> {Painting} <cult:technique> {Technique}
      using namespace 
	    cult = <!http://www.icom.com/schema.rdf#>',
  'Retrieve all painters and the paintings which they have painted, as well as the technique used in the painting').
q(4, 'select Artist, FName
      from   {Artist} <rdf:type> {<cult:Artist>};
		      [<cult:first_name> {FName}]
      using namespace 
	    cult = <!http://www.icom.com/schema.rdf#>',
  'Retrieve all artists and, if it is known, also their first name').
q(5, 'select Painter
      from   {Painter} <rdf:type> {<cult:Painter>}
      using namespace 
	    cult = <!http://www.icom.com/schema.rdf#>',
  'Retrieve all instances of the class \'Painter\'').
q(6, 'select X
      from   {<!http://www.european-history.com/picasso.html>} <rdf:type> {X}',
  'Retrieve all classes to which picasso belongs').
q(7, 'select X
      from   {<!http://www.european-history.com/picasso.html>} <serql:directType> {X}',
  'Retrieve the most specific classes to which picasso belongs').
q(8, 'select SUB
      from   {SUB} <rdfs:subClassOf> {<cult:Artist>}
      using namespace 
	      cult = <!http://www.icom.com/schema.rdf#>',
  'Retrieve all subclasses of the class \'Artist\'').
q(9, 'select DSUB
     from   {DSUB} <serql:directSubClassOf> {<cult:Artist>}
     using namespace 
	      cult = <!http://www.icom.com/schema.rdf#>',
  'Retrieve all direct subclasses of the class \'Artist\'').

% Comparison operators
% String comparison

q(10,'select Painter, FName
	    from   {Painter} <rdf:type> {<cult:Painter>}; 
	                     <cult:first_name> {FName}
	    where label(FName) = "Pablo"
	using namespace 
	      cult = <!http://www.icom.com/schema.rdf#>',
  'Retrieve all painters whose first name is \'Pablo\'').
q(11,'select Painter, FName
	    from   {Painter} <rdf:type> {<cult:Painter>}; 
	                     <cult:first_name> {FName}
	    where FName like "Pablo"
	using namespace 
	      cult = <!http://www.icom.com/schema.rdf#>',
  '(or) Retrieve all painters whose first name is \'Pablo\'').
q(12,'select Painter, FName
	    from   {Painter} <rdf:type> {<cult:Painter>}; 
	                     <cult:first_name> {FName}
	    where FName like "P*"
	using namespace 
	      cult = <!http://www.icom.com/schema.rdf#>',
  'Retrieve all painters whose first name starts with a \'P\'').
q(13,'select Class, DutchName
	    from   {Class} <rdf:type> {<rdfs:Class>};
	                   <rdfs:label> {DutchName}
 	    where  lang(DutchName) = "nl"
	    using namespace
	       adm = <!http://www.oclc.org/schema.rdf#>',
  'Retrieve all Dutch names of the classes in the schema').

% Numerical comparisons

q(14,'select Picture, FileSize
	    from   {Picture} <adm:file_size> {FileSize}
	    where  FileSize < "18"^^<xsd:int>
	    using namespace
	       adm = <!http://www.oclc.org/schema.rdf#>',
  'Retrieve all picture with a file size smaller than 18').

% Boolean operators

q(15,'select Painter, FName, LName
	    from   {Painter} <rdf:type> {<cult:Painter>}; 
	                     <cult:first_name> {FName};
			     <cult:last_name> {LName}
	    where not FName like "P*" and LName like "*Rijn*"
	using namespace 
	      cult = <!http://www.icom.com/schema.rdf#>',
  'Retrieve all painters whose first name does not start with a \'P\' and whose last name has the word \'Rijn\' in it.').

% Graph transformations

q(16,'construct *
	from   {<!http://www.european-history.com/picasso.html>} p {Y}',
  'Give me back all RDF statements about Picasso').
q(17,'construct 
	 {Painting} <my:createdBy> {Painter}
	from  
	 {Painter} <rdf:type> {<cult:Painter>}; 
	           <cult:creates> {Painting}
	using namespace 
	       cult = <!http://www.icom.com/schema.rdf#>,
	       my = <!http://www.foo.com/bar#>',
  ' Give me back all Painters and all the Paintings they created, but replace the property \'creates\' by a new inverse property \'foo:createdBy\' in the result.').
