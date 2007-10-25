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

:- module(test_manifest,
	  [ current_manifest/1,		% ?Manifest
	    current_test/2,		% ?Manifest, ?Test

	    test_name/2,		% ?Test, ?Name
	    test_query/2,		% +Test, -QueryCodes

	    test_query_file/2,		% +Test, -QueryFile
	    test_data_file/2,		% +Test, -DataFile
	    test_result_file/2,		% +Test, -ResultFile

					% EDIT
	    show_test/1,		% NameOrURL
	    edit_test/1,		% NameOrURL
	    show_test_data/1,		% NameOrURL
	    edit_test_data/1,		% NameOrURL
	    edit_test_result/1,		% NameOrURL

					% LOAD
	    load_manifests/1,		% +RootManifest

					% UTIL
	    load_triples_to_db/1,	% +FileOrURL
	    load_triples/2,		% +FileOrURL, -Triples
	    file_url/2			% ?File, ?URL
	  ]).
:- use_module(library(rdf)).
:- use_module(library(url)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(rdf_turtle).

:- dynamic
	mf_rdf/3.
:- rdf_meta
	mf_rdf(r, r, o).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Generic code to deal with W3C/Semantic web test manifests. These are RDF
files in either RDF/XML or Turtle format.

As we have to do reasoning on   the  complete graph and frequently clear
the graph during testing we keep  the   triples  for the manifest in the
Prolog database as mf_rdf(S,P,O).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


% namespaces used to process the manifests and test suite.
:- rdf_register_ns(mf,
		   'http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#').
:- rdf_register_ns(mfx,
		   'http://jena.hpl.hp.com/2005/05/test-manifest-extra#').
:- rdf_register_ns(qt,
		   'http://www.w3.org/2001/sw/DataAccess/tests/test-query#').
:- rdf_register_ns(r,
		   'http://www.w3.org/2001/sw/DataAccess/tests/result-set#').

% namespaces used in some of the test-suites to enhance readability.
:- rdf_register_ns(tbool,
		   'http://www.w3.org/2001/sw/DataAccess/tests/data/ValueTesting/boolean-0#').
:- rdf_register_ns(texttype,
		   'http://www.w3.org/2001/sw/DataAccess/tests/data/ValueTesting/extendedType-0#').
:- rdf_register_ns(tpromote, 'http://www.w3.org/2001/sw/DataAccess/tests/data/ValueTesting/typePromotion-0#').
:- rdf_register_ns(ex, 'http://example.org/#').


data_dir('Tests/sparql/data-xml').


		 /*******************************
		 *	      QUERY		*
		 *******************************/

%%	current_manifest(?Manifest)
%	
%	True if Manifest is a test-manifest

current_manifest(Manifest) :-
	mf_rdf(Manifest, rdf:type, mf:'Manifest').

%%	current_test(?Manifest, ?Test)
%	
%	True if Test is a test in Manifest.

current_test(Manifest, Test) :-
	current_manifest(Manifest),
	mf_rdf(Manifest, mf:entries, Collection),
	mf_member(Test, Collection).


%%	test_name(?Test, ?Name)
%	
%	True if Name is the name associated with Test.

test_name(Test, Name) :-
	var(Test), var(Name), !,
	mf_rdf(Test, mf:name, literal(Name)).
test_name(Test, Name) :-
	mf_rdf(Test, mf:name, literal(Name)), !.


%%	test_query_file(+Test, -File)
%	
%	Get the file containing the query of Test

test_query_file(Test, File) :-		% Normal cases
	mf_rdf(Test, mf:action, Action),
	mf_rdf(Action, qt:query, FileURI),
	parse_url(FileURI, Parts), !,
	memberchk(path(File), Parts).
test_query_file(Test, File) :-		% SyntaxDev cases
	mf_rdf(Test, mf:action, FileURI),
	parse_url(FileURI, Parts), !,
	memberchk(path(File), Parts).


%%	test_data_file(+Test, -File)
%	
%	Get the file containing the data for Test

test_data_file(Test, File) :-
	mf_rdf(Test, mf:action, Action),
	mf_rdf(Action, qt:data, FileURI),
	parse_url(FileURI, Parts), !,
	memberchk(path(File), Parts).


%%	test_result_file(+Test, -File)
%	
%	Test containg the result

test_result_file(Test, File) :-
	mf_rdf(Test, mf:result, FileURI),
	parse_url(FileURI, Parts), !,
	memberchk(path(File), Parts).


%%	test_query(+Test, -QueryCodes)

test_query(Test, Codes) :-
	test_query_file(Test, File), !,
	read_file_to_codes(File, Codes, []).
test_query(Test, _) :-
	format('Cannot get query for test ~q~n', [Test]),
	fail.


		 /*******************************
		 *	     LIST/EDIT		*
		 *******************************/

show_test(Name) :-
	test_name(Test, Name), !,
	show_test(Test).
show_test(Test) :-
	test_query(Test, Codes),
	format('~s', [Codes]).


edit_test(Name) :-
	test_name(Test, Name), !,
	edit_test(Test).
edit_test(Test) :-
	test_query_file(Test, File),
	edit(file(File)).

show_test_data(Name) :-
	test_name(Test, Name), !,
	show_test_data(Test).
show_test_data(Test) :-
	test_data_file(Test, File),
	read_file_to_codes(File, Codes, []),
	format('~s', [Codes]).

edit_test_data(Name) :-
	test_name(Test, Name), !,
	edit_test_data(Test).
edit_test_data(Test) :-
	test_data_file(Test, File),
	edit(file(File)).

edit_test_result(Name) :-
	test_name(Test, Name), !,
	edit_test_result(Test).
edit_test_result(Test) :-
	test_result_file(Test, File),
	edit(file(File)).


		 /************a*******************
		 *	  LOAD MANIFEST		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TBD: This should move in the   generic  semantic libraries, connected to
the core using a plugin extension mechanism.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	load_manifests(+RootOrAlias)
%	
%	Load the manifest files

load_manifests(dawg) :- !,
	data_dir(Dir),
	atom_concat(Dir, '/manifest.{ttl,rdf}', Pattern),
	expand_file_name(Pattern, [File]),
	file_url(File, URL),
	load_manifests(URL).
load_manifests(arq) :- !,
	file_url('../ARQ/testing/ARQ/manifest-arq.ttl', URL),
	load_manifests(URL).
load_manifests(Root) :-
	retractall(mf_rdf(_,_,_)),
	rdf_reset_db,
	load_schemas,
	load_manifest(Root),
	forall(rdf(S,P,O), assert(mf_rdf(S,P,O))),
	rdf_reset_db,
	test_statistics.

test_statistics :-
	findall(M, current_manifest(M), Ms),
	length(Ms, MN),
	findall(T, current_test(_, T), Ts),
	length(Ts, TN),
	format('Loaded ~D manifests with ~D tests~n', [MN, TN]).

load_schemas :-
	data_dir(Dir),
	atom_concat(Dir, '/test-manifest.rdf', S1),
	atom_concat(Dir, '/test-dawg.rdf', S2),
	atom_concat(Dir, '/test-query.rdf', S3),
	maplist(load_triples_to_db, [S1,S2,S3]).

load_manifest(URL) :-
	format('Loading ~w ...~n', [URL]),
	load_triples_to_db(URL),
	sub_manifests(URL, SubManifests),
	forall(member(S, SubManifests),
	       load_manifest(S)).

sub_manifests(URL, List) :-
	rdf(URL, mfx:include, Collection), !,
	findall(S, rdfs_member(S, Collection), List).
sub_manifests(_, []).



		 /*******************************
		 *	     LOAD FILES		*
		 *******************************/

%%	load_triples_to_db(+File)
%	
%	Load triples from a file into a the RDF database

load_triples_to_db(URL) :-
	file_url(File, URL), !,
	load_triples_to_db(File).
load_triples_to_db(File) :-
	file_name_extension(_, Ext, File),
	(   load_triples_to_db(Ext, File)
	->  true
	;   assert(user:load_failed(File))
	).

load_triples_to_db(rdf, File) :- !,	% RDF/XML
	rdf_load(File, [silent(true)]).
load_triples_to_db(ttl, File) :- !,	% Turtle
	absolute_file_name(File, Path),
	rdf_load_turtle_file(File, Triples, []),
	maplist(assert_triple(Path), Triples).
load_triples_to_db(n3, File) :- !,	% Turtle
	absolute_file_name(File, Path),
	rdf_load_turtle_file(File, Triples, []),
	maplist(assert_triple(Path), Triples).

assert_triple(DB, rdf(S,P,O)) :-
	rdf_assert(S,P,O, DB).

%%	load_triples(+File, -RDFList)
%	
%	Load file into a list of rdf(S,P,O) terms

load_triples(File, Triples) :-
	file_name_extension(_, Ext, File),
	load_triples(Ext, File, Triples).

load_triples(rdf, File, Triples) :- !,
	load_rdf(File, Triples).
load_triples(ttl, File, Triples) :- !,
	rdf_load_turtle_file(File, Triples, []).


		 /*******************************
		 *		URL		*
		 *******************************/

file_url(File, URL) :-
	ground(File), !,
	absolute_file_name(File, Path),
	atom_concat('file://', Path, URL).
file_url(File, URL) :-
	atom_concat('file://', File, URL).


		 /*******************************
		 *		UTIL		*
		 *******************************/

mf_member(Element, Set) :-
	mf_rdf(Set, rdf:first, Element).
mf_member(Element, Set) :-
	mf_rdf(Set, rdf:rest, Tail),
	mf_member(Element, Tail).
