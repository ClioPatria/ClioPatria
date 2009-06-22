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
	    test_syntax/2,		% +Test, ?PosNeg

	    test_query_file/2,		% +Test, -QueryFile
	    test_data_files/2,		% +Test, -DataFiles
	    test_result_file/2,		% +Test, -ResultFile

					% EDIT
	    show_test/1,		% NameOrURL
	    edit_test/1,		% NameOrURL
	    show_test_data/1,		% NameOrURL
	    edit_test_data/1,		% NameOrURL
	    edit_test_result/1,		% NameOrURL

					% LOAD
	    load_manifests/1,		% +RootManifest
	    reset_manifests/0,

					% UTIL
	    load_triples/2,		% +FileOrURL, -Triples
	    file_url/2			% ?File, ?URL
	  ]).
:- use_module(library(rdf)).
:- use_module(library(url)).
:- use_module(library(apply)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_turtle')).

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


%%	test_name(?Test, ?Name) is det.
%
%	True if Name is the name associated with Test.

test_name(Test, Name) :-
	var(Test), var(Name), !,
	mf_rdf(Test, mf:name, literal(Name)).
test_name(Test, Name) :-
	mf_rdf(Test, mf:name, literal(Name)), !.

%%	test_syntax(+Test, ?PosNeg) is semidet.
%
%	True if Test is a syntax-test.

test_syntax(Test, positive) :-
	(   mf_rdf(Test, rdf:type, mfx:'TestSyntax')
	;   mf_rdf(Test, rdf:type, mf:'PositiveSyntaxTest')
	), !.
test_syntax(Test, positive) :-
	mf_rdf(Test, mf:action, Action),
        mf_rdf(Action, qt:query, _), !.
test_syntax(Test, negative) :-
	(   mf_rdf(Test, rdf:type, mfx:'TestBadSyntax')
	;   mf_rdf(Test, rdf:type, mf:'NegativeSyntaxTest')
	), !.


%%	test_query_file(+Test, -File)
%
%	Get the file containing the query of Test

test_query_file(Test, File) :-		% Normal cases
	mf_rdf(Test, mf:action, Action),
	mf_rdf(Action, qt:query, FileURI), !,
	file_name_to_url(File, FileURI).
test_query_file(Test, File) :-		% SyntaxDev cases
	mf_rdf(Test, mf:action, FileURI), !,
	file_name_to_url(File, FileURI).


%%	test_data_files(+Test, -Files)
%
%	Get the files containing the data for Test

test_data_files(Test, Files) :-
	setof(File, test_data_file(Test, File), Files).

test_data_file(Test, File) :-
	mf_rdf(Test, mf:action, Action),
	mf_rdf(Action, qt:data, FileURI),
	file_name_to_url(File, FileURI).


%%	test_result_file(+Test, -File)
%
%	Test containg the result

test_result_file(Test, File) :-
	mf_rdf(Test, mf:result, FileURI),
	file_name_to_url(File, FileURI).


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
	test_data_files(Test, Files),
	forall(member(File, Files),
	       edit(file(File))).

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

load_manifests(Spec) :-
	reset_manifests,
	load_schemas('Tests/sparql/test-suite-archive'),
	do_load_manifests(Spec),
	forall(rdf(S,P,O), assert(mf_rdf(S,P,O))),
	rdf_reset_db,
	test_statistics.

do_load_manifests([]) :- !.
do_load_manifests([H|T]) :- !,
	do_load_manifests(H),
	do_load_manifests(T).
do_load_manifests(dawg) :- !,
	do_load_manifests('Tests/sparql/test-suite-archive/data-r2/manifest-evaluation').
do_load_manifests(arq) :- !,
	file_url('Tests/sparql/ARQ/manifest-arq.ttl', URL),
	do_load_manifests(URL).
do_load_manifests(Root) :-
	to_url(Root, URL),
	load_manifest(URL).

to_url(URL, URL) :-
	sub_atom(URL, 0, _, _, 'file://'), !.
to_url(File, URL) :-
	file_url(File, URL).

reset_manifests :-
	retractall(mf_rdf(_,_,_)),
	rdf_reset_db.

test_statistics :-
	findall(M, current_manifest(M), Ms),
	length(Ms, MN),
	findall(T, current_test(_, T), Ts),
	length(Ts, TN),
	format('Loaded ~D manifests with ~D tests~n', [MN, TN]).

load_schemas(Dir) :-
	atom_concat(Dir, '/test-manifest', S1),
	atom_concat(Dir, '/test-dawg', S2),
	atom_concat(Dir, '/test-query', S3),
	maplist(rdf_load, [S1,S2,S3]).

load_manifest(URL) :-
	format('Loading ~w ...~n', [URL]),
	rdf_load(URL),
	sub_manifests(URL, SubManifests),
	forall(member(S, SubManifests),
	       load_manifest(S)).

sub_manifests(URL, List) :-
	findall(X, includes(URL, X), List0),
	sort(List0, List).

includes(URL, Sub) :-
	(   rdf(URL, mfx:include, Collection) % old
	;   rdf(URL, mf:include, Collection)  % new
	),
	rdfs_member(Sub, Collection).


		 /*******************************
		 *	     LOAD FILES		*
		 *******************************/

%%	load_triples(+File, -RDFList)
%
%	Load file into a list of rdf(S,P,O) terms

load_triples(File, Triples) :-
	file_name_extension(_, Ext, File),
	load_triples(Ext, File, Triples).

load_triples(rdf, File, Triples) :- !,
	load_rdf(File, Triples).
load_triples(ttl, File, Triples) :- !,
	rdf_load_turtle(File, Triples, []).
load_triples(n3, File, Triples) :- !,
	rdf_load_turtle(File, Triples, []).


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
