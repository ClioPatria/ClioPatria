/*  Part of ClioPatria

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(test_manifest,
          [ current_manifest/1,         % ?Manifest
            current_test/2,             % ?Manifest, ?Test

            test_name/2,                % ?Test, ?Name
            test_query/2,               % +Test, -QueryCodes
            test_syntax/2,              % +Test, ?PosNeg
            test_entailment/2,          % +Test, -Entailment
            test_info/1,                % +TestOrName

            test_query_file/2,          % +Test, -QueryFile
            test_data_files/2,          % +Test, -DataFiles
            test_result_file/2,         % +Test, -ResultFile
            test_result_graph/3,        % +Test, -ResultFile, -Graph

                                        % EDIT
            show_test/1,                % NameOrURL
            edit_test/1,                % NameOrURL
            show_test_data/1,           % NameOrURL
            edit_test_data/1,           % NameOrURL
            edit_test_result/1,         % NameOrURL

                                        % LOAD
            load_manifests/1,           % +RootManifest
            reset_manifests/0,

                                        % UTIL
            load_triples/3,             % +FileOrURL, -Triples, +Options
            file_url/2                  % ?File, ?URL
          ]).
:- use_module(library(rdf)).
:- use_module(library(url)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_turtle_write)).

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
:- rdf_register_ns(ut,
                   'http://www.w3.org/2009/sparql/tests/test-update#').
:- rdf_register_ns(sd,
                   'http://www.w3.org/ns/sparql-service-description#').
:- rdf_register_ns(r,
                   'http://www.w3.org/2001/sw/DataAccess/tests/result-set#').
:- rdf_register_ns(entailment,
                   'http://www.w3.org/ns/entailment/').

% namespaces used in some of the test-suites to enhance readability.
:- rdf_register_ns(tbool,
                   'http://www.w3.org/2001/sw/DataAccess/tests/data/ValueTesting/boolean-0#').
:- rdf_register_ns(texttype,
                   'http://www.w3.org/2001/sw/DataAccess/tests/data/ValueTesting/extendedType-0#').
:- rdf_register_ns(tpromote, 'http://www.w3.org/2001/sw/DataAccess/tests/data/ValueTesting/typePromotion-0#').
:- rdf_register_ns(ex, 'http://example.org/#').


                 /*******************************
                 *            QUERY             *
                 *******************************/

%!  current_manifest(?Manifest)
%
%   True if Manifest is a test-manifest

current_manifest(Manifest) :-
    mf_rdf(Manifest, rdf:type, mf:'Manifest').

%!  current_test(?Manifest, ?Test)
%
%   True if Test is a test in Manifest.

current_test(Manifest, Test) :-
    current_manifest(Manifest),
    mf_rdf(Manifest, mf:entries, Collection),
    mf_member(Test, Collection).


%!  test_name(?Test, ?Name) is nondet.
%
%   True if Name is the name associated with Test.

test_name(Test, Name) :-
    var(Test), var(Name),
    !,
    mf_rdf(Test, mf:name, literal(Name)).
test_name(Test, Name) :-
    mf_rdf(Test, mf:name, literal(Name)),
    !.

%!  test_syntax(+Test, ?PosNeg) is semidet.
%
%   True if Test is a syntax-test.

test_syntax(Test, PosNeg) :-
    (   mf_rdf(Test, rdf:type, mfx:'TestSyntax')
    ;   mf_rdf(Test, rdf:type, mf:'PositiveSyntaxTest')
    ;   mf_rdf(Test, rdf:type, mf:'PositiveSyntaxTest11')
    ;   mf_rdf(Test, rdf:type, mf:'PositiveUpdateSyntaxTest11')
    ),
    !,
    PosNeg = positive.
test_syntax(Test, PosNeg) :-
    (   mf_rdf(Test, rdf:type, mfx:'TestBadSyntax')
    ;   mf_rdf(Test, rdf:type, mf:'NegativeSyntaxTest')
    ;   mf_rdf(Test, rdf:type, mf:'NegativeSyntaxTest11')
    ;   mf_rdf(Test, rdf:type, mf:'NegativeUpdateSyntaxTest11')
    ),
    !,
    PosNeg = negative.
test_syntax(Test, query) :-
    test_query_file(Test, _),
    !.


%!  test_info(+Test) is det.
%
%   Dump basic information about the test as RDF triples.

test_info(Name) :-
    test_name(Test, Name),
    !,
    test_info(Test).
test_info(Test) :-
    test_triples([Test], Triples, []),
    rdf_save_turtle(stream(current_output), [ expand(triple_in(Triples)) ]).

test_triples([], Triples, Triples).
test_triples([H|T], Triples, Tail) :-
    findall(rdf(H,P,O), mf_rdf(H,P,O), Triples, Tail0),
    objects(Triples, O),
    append(T, O, List),
    test_triples(List, Tail0, Tail).

objects(Var, []) :-
    var(Var),
    !.
objects([rdf(_,_,O)|T0], [O|T]) :-
    atom(O),
    !,
    objects(T0, T).
objects([_|T0], T) :-
    objects(T0, T).

triple_in(RDF, S,P,O,_G) :-
    member(rdf(S,P,O), RDF).


%!  test_query_file(+Test, -File)
%
%   Get the file containing the query of Test

test_query_file(Test, File) :-          % Normal cases
    mf_rdf(Test, mf:action, Action),
    (   mf_rdf(Action, qt:query, FileURI)
    ;   mf_rdf(Action, ut:request, FileURI)
    ),
    !,
    file_name_to_url(File, FileURI).
test_query_file(Test, File) :-          % SyntaxDev cases
    mf_rdf(Test, mf:action, FileURI),
    !,
    file_name_to_url(File, FileURI).


%!  test_entailment(+Test, -Entailment) is semidet.
%
%   Entailment regime to use for the test.

test_entailment(Test, Entailment) :-
    mf_rdf(Test, mf:action, Action),
    mf_rdf(Action, sd:entailmentRegime, Entailment),
    !.


%!  test_data_files(+Test, -Files)
%
%   Get the files containing the data for Test.  Files is a list
%   File-Graph.

test_data_files(Test, Files) :-
    setof(File, test_data_file(Test, File), Files).

test_data_file(Test, File-Graph) :-
    mf_rdf(Test, mf:action, Action),
    (   Graph = user,
        (   mf_rdf(Action, qt:data, FileURI)
        ;   mf_rdf(Action, ut:data, FileURI)
        )
    ;   (   mf_rdf(Action, qt:graphData, FileURI)
        ;   mf_rdf(Action, ut:graphData, FileURI)
        ),
        file_base_name(FileURI, BaseName),
        graph_base_uri(BaseURI),
        uri_normalized(BaseName, BaseURI, Graph)
    ),
    file_name_to_url(File, FileURI).
test_data_file(Test, File-Graph) :-
    mf_rdf(Test, mf:action, Action),
    mf_rdf(Action, ut:graphData, GraphData),
    mf_rdf(GraphData, rdfs:label, literal(Graph)),
    mf_rdf(GraphData, ut:graph, FileURL),
    file_name_to_url(File, FileURL).

graph_base_uri('http://default.base.org/').

%!  test_result_file(+Test, -File)
%
%   Test containg the result

test_result_file(Test, File) :-
    mf_rdf(Test, mf:result, FileURI),
    file_name_to_url(File, FileURI).

%!  test_result_graph(+Test, -File, -Graph)
%
%   Test results per graph

test_result_graph(Test, File, user) :-
    mf_rdf(Test, mf:result, ResultNode),
    mf_rdf(ResultNode, ut:data, FileURL),
    file_name_to_url(File, FileURL).
test_result_graph(Test, File, Graph) :-
    mf_rdf(Test, mf:result, ResultNode),
    mf_rdf(ResultNode, ut:graphData, GraphData),
    mf_rdf(GraphData, rdf:label, literal(Graph)),
    mf_rdf(GraphData, ut:graph, FileURL),
    file_name_to_url(File, FileURL).


%!  test_query(+Test, -QueryCodes)

test_query(Test, Codes) :-
    test_query_file(Test, File),
    !,
    read_file_to_codes(File, Codes, []).
test_query(Test, _) :-
    debug(no_query, 'Cannot get query for test ~q~n', [Test]),
    fail.


                 /*******************************
                 *           LIST/EDIT          *
                 *******************************/

show_test(Name) :-
    test_name(Test, Name),
    !,
    show_test(Test).
show_test(Test) :-
    test_query(Test, Codes),
    format('~s', [Codes]).


edit_test(Name) :-
    test_name(Test, Name),
    !,
    edit_test(Test).
edit_test(Test) :-
    test_query_file(Test, File),
    edit(file(File)).

show_test_data(Name) :-
    test_name(Test, Name),
    !,
    show_test_data(Test).
show_test_data(Test) :-
    test_data_file(Test, File-_Graph),
    !,
    read_file_to_codes(File, Codes, []),
    format('~s', [Codes]).

edit_test_data(Name) :-
    test_name(Test, Name),
    !,
    edit_test_data(Test).
edit_test_data(Test) :-
    test_data_files(Test, Files),
    forall(member(File-_Graph, Files),
           edit(file(File))).

edit_test_result(Name) :-
    test_name(Test, Name),
    !,
    edit_test_result(Test).
edit_test_result(Test) :-
    test_result_file(Test, File),
    edit(file(File)).


                 /*******************************
                 *        LOAD MANIFEST         *
                 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TBD: This should move in the   generic  semantic libraries, connected to
the core using a plugin extension mechanism.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%!  load_manifests(+RootOrAlias)
%
%   Load the manifest files

load_manifests(Spec) :-
    reset_manifests,
    load_schemas('Tests/sparql/test-suite-archive'),
    do_load_manifests(Spec),
    forall(rdf(S,P,O), assert(mf_rdf(S,P,O))),
    rdf_reset_db,
    test_statistics.

do_load_manifests([]) :- !.
do_load_manifests([H|T]) :-
    !,
    do_load_manifests(H),
    do_load_manifests(T).
do_load_manifests(dawg) :-
    !,
    do_load_manifests('Tests/sparql/test-suite-archive/data-r2/manifest-evaluation').
do_load_manifests(arq) :-
    !,
    file_url('Tests/sparql/ARQ/manifest-arq.ttl', URL),
    do_load_manifests(URL).
do_load_manifests(sparql11) :-
    !,
    do_load_manifests('Tests/sparql-1.1/2009/sparql/docs/tests/data-sparql11/manifest-all.ttl').
do_load_manifests(Root) :-
    to_url(Root, URL),
    load_manifest(URL).

to_url(URL, URL) :-
    sub_atom(URL, 0, _, _, 'file://'),
    !.
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
                 *           LOAD FILES         *
                 *******************************/

%!  load_triples(+File, -RDFList, +Options)
%
%   Load file into a list of rdf(S,P,O) terms

load_triples(File, Triples, Options) :-
    file_name_extension(_, Ext, File),
    load_triples(Ext, File, Triples, Options).

load_triples(rdf, File, Triples, Options) :-
    !,
    load_rdf(File, Triples, Options).
load_triples(ttl, File, Triples, Options) :-
    !,
    rdf_load_turtle(File, Triples, Options).
load_triples(n3, File, Triples, Options) :-
    !,
    rdf_load_turtle(File, Triples, Options).


                 /*******************************
                 *              URL             *
                 *******************************/

file_url(File, URL) :-
    ground(File),
    !,
    absolute_file_name(File, Path),
    atom_concat('file://', Path, URL).
file_url(File, URL) :-
    atom_concat('file://', File, URL).


                 /*******************************
                 *              UTIL            *
                 *******************************/

mf_member(Element, Set) :-
    mf_rdf(Set, rdf:first, Element).
mf_member(Element, Set) :-
    mf_rdf(Set, rdf:rest, Tail),
    mf_member(Element, Tail).
