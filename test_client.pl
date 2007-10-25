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

:- module(test_client,
	  [ graph/2,			% +QID, -Triple
	    table/2,			% +QID, -Row

	    count/0,			% Count triples

	    list_repositories/0,
	    clear/0,			% Clear all content

	    login/0,
	    login/2,			% +User, +Password
	    logout/0,

	    upload/1,			% Upload File
	    upload/2,			% File, Options
	    add/1,			% +Triples
	    del/1,			% +Triple
	    wns/0,			% Upload wordnet

	    sesame/0,			% Select Java server
	    local/0			% Use SWI-Prolog server
	  ]).

:- load_files(sesame_client,
	      [ silent(true)
	      ]).

:- style_check(-atom).

local :-
	set_sesame_default([ host(localhost),
			     port(3020),
			     path(''),
			     repository(default)
			   ]).

sesame :-
	set_sesame_default([ host(gollem),
			     port(4242),
			     path('/sesame'),
			     repository('mem-rdfs-db')
			   ]).

:- local.


		 /*******************************
		 *	     QUERIES	 	*
		 *******************************/

qg(1, 'construct distinct * from {S} P {O}').
qg(2, 'construct * from {<rdfs:Resource>} <rdfs:label> {L}').

graph(QueryID, Triple) :-
	qg(QueryID, Query),
	sesame_graph_query(Query,
			   Triple,
			   []).

count :-
	qg(1, Query),
	findall(T, sesame_graph_query(Query, T, []), Ts),
	length(Ts, Len),
	format('~D triples in store~n', [Len]).

qt(1, 'select distinct S from {S} P {O} where O like "Resource"').

% Give me wordnet synsets labeled `right' of type Noun
qt(right_noun,
  'SELECT S
   FROM {S} <rdfs:label> {L} ;
	    <rdf:type> {<wns:Noun>}
   WHERE L like "right"
   USING NAMESPACE
	 wns = <!http://www.cogsci.princeton.edu/~wn/schema/>').
        
qt(right_noun_r,
  'SELECT S
   FROM {S} <rdf:type> {<wns:Noun>} ;
            <rdfs:label> {L}
   WHERE L like "right"
   USING NAMESPACE
	 wns = <!http://www.cogsci.princeton.edu/~wn/schema/>').
        
% Give me a WordNet word that belongs to multiple lexical categories
qt(multi_lex,
  'SELECT DISTINCT L
   FROM {S1} <rdfs:label> {L},
        {S2} <rdfs:label> {L},
	{S1} <rdf:type> {C1},
	{S2} <rdf:type> {C2},
	{C1} <serql:directSubClassOf> {<wns:LexicalConcept>},
	{C2} <serql:directSubClassOf> {<wns:LexicalConcept>}
   WHERE not C1 = C2
   USING NAMESPACE
	 wns = <!http://www.cogsci.princeton.edu/~wn/schema/>').

table(QueryID, Row) :-
	qt(QueryID, Query),
	sesame_table_query(Query,
			   Row,
			   []).


		 /*******************************
		 *	   REPOSITORIES		*
		 *******************************/

list_repositories :-
	(   sesame_current_repository(Id, Atts, []),
	    format('~w ~w~n', [Id, Atts]),
	    fail
	;   true
	).

clear :-
	sesame_clear_repository([]).


		 /*******************************
		 *		LOGIN		*
		 *******************************/

%	login
%	
%	Read facts from a file secrets.pl holing lines of the format
%	
%%		secret(Host:Port, User, Password).

login :-
	sesame_client:sesame_setting(host(Host)),
	sesame_client:sesame_setting(port(Port)),
	read_file_to_terms('secrets.pl', Terms, []),
	memberchk(secret(Host:Port, User, Password), Terms),
	login(User, Password).


login(User, Password) :-
	sesame_login(User, Password, []).

logout :-
	sesame_logout([]).


		 /*******************************
		 *	       UPLOAD		*
		 *******************************/

upload(rdfs) :-
	sesame_upload_file('Ontologies/Base/rdfs.rdfs',
			   [ base_uri('rdfs.rdfs')
			   ]).
upload(wine) :-
	sesame_upload_file('Ontologies/Demo/wine.owl',
			   [ base_uri('wine')
			   ]).
upload(File) :-
	upload(File, []).

upload(File, Options) :-
	absolute_file_name(File,
			   [ extensions([rdf,rdfs]),
			     access(read)
			   ],
			   Path),
	sesame_upload_file(Path, Options).


add(Triples) :-
	sesame_assert(Triples, []).

del(Triple) :-
	sesame_retract(Triple, []).

wn_file('/staff/jan/src/Triple20/Ontologies/Public/wordnet-20000620.rdfs').
wn_file('/staff/jan/src/Triple20/Ontologies/Public/wordnet_hyponyms-20010201.rdf').
wn_file('/staff/jan/src/Triple20/Ontologies/Public/wordnet_similar-20010201.rdf').
wn_file('/staff/jan/src/Triple20/Ontologies/Public/wordnet_nouns-20010201.rdf').
wn_file('/staff/jan/src/Triple20/Ontologies/Public/wordnet_glossary-20010201.rdf').
wn_file('/staff/jan/src/Triple20/Ontologies/Public/wnclass.rdfs').

wns :-
	clear,
	forall(wn_file(File),
	       (   format(user_error, 'Uploading ~w ...', [File]),
		   sesame_upload_file(File, []),
		   format(user_error, 'done~n', []))).
	
