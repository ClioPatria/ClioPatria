/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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


:- module(sesame_http_body,
	  [ stored_query/4,		% Name, User, Type, Query
	    serql_base_ontology/1	% -Ontology
	  ]).
:- use_module(parms).
:- use_module(serql).
:- use_module(sparql).
:- use_module(library('http/http_parameters')).
:- use_module(http_admin).
:- use_module(user_db).
:- use_module(rdf_html).
:- use_module(xml_result).
:- use_module(rdf_io).
:- use_module(library('semweb/rdf_edit')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library('http/html_write')).
:- use_module(library('http/http_open')).
:- use_module(library('http/http_dispatch')).
:- use_module(library(memfile)).
:- use_module(library(rdf_ntriples)).
:- use_module(library(debug)).
:- use_module(library(settings)).
:- use_module(rdf_store).

					% entailment rules
:- use_module(rdf_entailment, []).
:- use_module(rdfs_entailment, []).
:- use_module(rdfslite_entailment, []).

:- http_handler(sesame('login'),	      http_login,	    []).
:- http_handler(sesame('logout'),	      http_logout,	    []).
:- http_handler(sesame('evaluateQuery'),      evaluate_query,	    [spawn(sparql_query)]).
:- http_handler(sesame('evaluateGraphQuery'), evaluate_graph_query, [spawn(sparql_query)]).
:- http_handler(sesame('evaluateTableQuery'), evaluate_table_query, [spawn(sparql_query)]).
:- http_handler(sesame('extractRDF'),	      extract_rdf,	    []).
:- http_handler(sesame('listRepositories'),   list_repositories,    []).
:- http_handler(sesame('clearRepository'),    clear_repository,	    []).
:- http_handler(sesame('loadBaseOntology'),   load_base_ontology,   []).
:- http_handler(sesame('listBaseOntologies'), list_base_ontologies, []).
:- http_handler(sesame('unloadSource'),	      unload_source,
		[ time_limit(infinite) ]).
:- http_handler(sesame('uploadData'),	      upload_data,
		[ time_limit(infinite) ]).
:- http_handler(sesame('uploadFile'),	      upload_file,
		[ time_limit(infinite) ]).
:- http_handler(sesame('uploadURL'),	      upload_url,
		[ time_limit(infinite) ]).
:- http_handler(sesame('removeStatements'),   remove_statements,
		[ time_limit(infinite) ]).

%%	http_login(+Request)
%
%	HTTP handler for SeRQL login.

http_login(Request) :-
	http_parameters(Request,
			[ user(User, []),
			  password(Password, [])
			]),
	validate_password(User, Password),
	login(User),
	reply_page('Successful login',
		   p(['Login succeeded for ', User])).

%%      http_logout(+Request)
%
%       HTTP handler to logout current user.

http_logout(_Request) :-
	logged_on(User),
	logout(User),
	reply_page('Successful logout',
		   p(['Logout succeeded for ', User])).

%%	evaluate_query
%
%	HTTP handler for both SeRQL and SPARQL queries.

evaluate_query(Request) :-
	http_parameters(Request,
			[ repository(Repository),
			  query(Query),
			  queryLanguage(QueryLanguage),
			  resultFormat(ResultFormat),
			  serialization(Serialization),
			  resourceFormat(ResourceFormat),
			  entailment(Entailment),
			  storeAs(SaveAs)
			],
			[ attribute_declarations(attribute_decl)
			]),
	authorized(read(Repository, query)),
	statistics(cputime, CPU0),
	downcase_atom(QueryLanguage, QLang),
	compile(QLang, Query, Compiled,
		[ entailment(Entailment),
		  type(Type)
		]),
	findall(Reply, run(QLang, Compiled, Reply), Result),
	statistics(cputime, CPU1),
	CPU is CPU1 - CPU0,
	store_query(construct, SaveAs, Query),
	(   graph_type(Type)
	->  write_graph(Result,
			[ result_format(ResultFormat),
			  serialization(Serialization),
			  resource_format(ResourceFormat),
			  cputime(CPU)
			])
	;   Type = select(VarNames)
	->  write_table(Result,
			[ variables(VarNames),
			  result_format(ResultFormat),
			  serialization(Serialization),
			  resource_format(ResourceFormat),
			  cputime(CPU)
			])
	;   Type == ask
	->  reply_page('ASK Result',
		       [ h4('ASK query completed'),
			 p(['Answer = ', Reply])
		       ])
	).


%%	evaluate_graph_query(+Request)
%
%	Handle CONSTRUCT queries.

evaluate_graph_query(Request) :-
	http_parameters(Request,
			[ repository(Repository),
			  query(Query),
			  queryLanguage(QueryLanguage),
			  resultFormat(ResultFormat),
			  serialization(Serialization),
			  resourceFormat(ResourceFormat),
			  entailment(Entailment),
			  storeAs(SaveAs)
			],
			[ attribute_declarations(attribute_decl)
			]),
	authorized(read(Repository, query)),
	statistics(cputime, CPU0),
	downcase_atom(QueryLanguage, QLang),
	compile(QLang, Query, Compiled,
		[ entailment(Entailment),
		  type(Type)
		]),
	(   graph_type(Type)
	->  true
	;   throw(error(domain_error(query_type(graph), Type), _))
	),
	findall(T, run(QLang, Compiled, T), Triples),
	statistics(cputime, CPU1),
	store_query(construct, SaveAs, Query),
	CPU is CPU1 - CPU0,
	write_graph(Triples,
		    [ result_format(ResultFormat),
		      serialization(Serialization),
		      resource_format(ResourceFormat),
		      cputime(CPU)
		    ]).

graph_type(construct).
graph_type(describe).

%%	evaluate_table_query(+Request)
%
%	Handle SELECT queries.

evaluate_table_query(Request) :-
	http_parameters(Request,
			[ repository(Repository),
			  query(Query),
			  queryLanguage(QueryLanguage),
			  resultFormat(ResultFormat),
			  serialization(Serialization),
			  resourceFormat(ResourceFormat),
			  entailment(Entailment),
			  storeAs(SaveAs)
			],
			[ attribute_declarations(attribute_decl)
			]),
	authorized(read(Repository, query)),
	statistics(cputime, CPU0),
	downcase_atom(QueryLanguage, QLang),
	compile(QLang, Query, Compiled,
		[ entailment(Entailment),
		  type(select(VarNames))
		]),
	findall(R, run(QLang, Compiled, R), Rows),
	statistics(cputime, CPU1),
	CPU is CPU1 - CPU0,
	store_query(select, SaveAs, Query),
	write_table(Rows,
		    [ variables(VarNames),
		      result_format(ResultFormat),
		      serialization(Serialization),
		      resource_format(ResourceFormat),
		      cputime(CPU)
		    ]).

%%	compile(+Language, +Query, -Compiled, +Options)
%
%	Compile a query and validate the query-type

compile(serql, Query, Compiled, Options) :- !,
	serql_compile(Query, Compiled, Options).
compile(sparql, Query, Compiled, Options) :- !,
	sparql_compile(Query, Compiled, Options).
compile(Language, _, _, _) :-
	throw(error(domain_error(query_language, Language), _)).

%%	run(+Language, +Compiled, -Reply)

run(serql, Compiled, Reply) :-
	serql_run(Compiled, Reply).
run(sparql, Compiled, Reply) :-
	sparql_run(Compiled, Reply).

%%	extract_rdf(+Request)
%
%	HTTP handler to extract RDF from the database.

extract_rdf(Request) :-
	http_parameters(Request,
			[ repository(_Repository),
			  schema(Schema),
			  data(Data),
			  explicitOnly(ExplicitOnly),
			  niceOutput(_NiceOutput),
			  serialization(Serialization)
			],
			[ attribute_declarations(attribute_decl)
			]),
	statistics(cputime, CPU0),
	findall(T, export_triple(Schema, Data, ExplicitOnly, T), Triples),
	statistics(cputime, CPU1),
	CPU is CPU1 - CPU0,
	write_graph(Triples,
		    [ serialization(Serialization),
		      cputime(CPU)
		    ]).


%%	export_triple(+Schema, +Data, +ExplicitOnly, -RDF).

export_triple(off, off, _, _) :- !,
	fail.				% no data requested
export_triple(on, on, on, rdf(S,P,O)) :- !,
	rdf_db:rdf(S,P,O).
export_triple(on, on, off, rdf(S,P,O)) :- !,
	rdfs_entailment:rdf(S,P,O).
export_triple(off, on, Explicit, RDF) :-
	export_triple(on, on, Explicit, RDF),
	\+ schema_triple(RDF).
export_triple(on, off, Explicit, RDF) :-
	export_triple(on, on, Explicit, RDF),
	schema_triple(RDF).

schema_triple(rdf(S,_P,_O)) :-
	rdfs_individual_of(S, rdfs:'Property').
schema_triple(rdf(S,_P,_O)) :-
	rdfs_individual_of(S, rdfs:'Class').


%%	list_repositories(+Request)
%
%	List the available repositories. This is only =default= for now

list_repositories(_Request) :-
	Repository = default,
	logged_on(User, anonymous),
	(   catch(check_permission(User, write(Repository, _)), _, fail)
	->  Write = true
	;   Write = false
	),
	(   catch(check_permission(User, read(Repository, _)), _, fail)
	->  Read = true
	;   Read = false
	),
	format('Content-type: text/xml~n~n'),
	format('<?xml version="1.0" encoding="ISO-8859-1"?>~n~n', []),
	format('<repositorylist>~n'),
	format('  <repository id="default" readable="~w" writeable="~w">~n',
	       [ Read, Write ]),
	format('    <title>Default repository</title>~n'),
	format('  </repository>~n'),
	format('</repositorylist>~n').


%%	clear_repository(+Request)
%
%	Clear the repository.

clear_repository(Request) :-
	http_parameters(Request,
			[ repository(Repository),
			  resultFormat(Format)
			],
			[ attribute_declarations(attribute_decl)
			]),
	authorized(write(Repository, clear)),
	action(Request,
	       (  rdf_reset_db,		% must be in seperate transactions
		  rdf_load(serql('rdfs'))
	       ),
	       Format,
	       'Cleared database'-[]).

%%	load_base_ontology(+Request)
%
%	Load a named ontology from the ontology library.
%
%	@tbd	Cannot use concurrent loading as the load as a whole is
%		inside an rdf transaction.

load_base_ontology(Request) :-
	http_parameters(Request,
			[ repository(Repository),
			  ontology(Ontology, []),
			  resultFormat(Format)
			],
			[ attribute_declarations(attribute_decl)
			]),
	authorized(write(Repository, load(base_ontology(Ontology)))),
	prepare_ontology_dirs,
	action(Request,
	       rdf_load_library(Ontology, [concurrent(1)]),
	       Format,
	       'Loaded base ontology ~w'-[Ontology]).


%%	list_base_ontologies(+Request)
%
%	Reply with a list of available base ontologies

list_base_ontologies(Request) :-
	authorized(read(status, listBaseOntologies)),
	http_parameters(Request,
			[ resultFormat(Format),
			  serialization(Serialization)
			],
			[ attribute_declarations(attribute_decl)
			]),
	catch(findall(row(O), serql_base_ontology(O), Rows), _,
	      Rows = []),
	write_table(Rows,
		    [ result_format(Format),
		      serialization(Serialization),
		      variables(varnames(ontology))
		    ]).


%%	serql_base_ontology(-Name) is nondet.
%
%	True if Name is the name of an ontology from the library.
%
%	@deprecated	Use rdf_library_index/2.

serql_base_ontology(O) :-
	prepare_ontology_dirs,
	rdf_library_index(O, title(_Title)).


%%	prepare_ontology_dirs is det.
%
%	Load RDF library manifests from   directories  defined using the
%	file_search_path/2 =ontology_root= alias.

prepare_ontology_dirs :-
	(   absolute_file_name(ontology_root(.), Dir,
			       [ file_type(directory),
				 solutions(all)
			       ]),
	    rdf_attach_library(Dir),
	    fail
	;   true
	).

%%	unload_source(+Request)
%
%	Remove triples loaded from a specified source

unload_source(Request) :-
	http_parameters(Request,
			[ repository(Repository),
			  source(Source, []),
			  resultFormat(Format)
			],
			[ attribute_declarations(attribute_decl)
			]),
	authorized(write(Repository, unload(Source))),
	action(Request, rdf_unload(Source),
	       Format,
	       'Unloaded triples from ~w'-[Source]).


%%	upload_data(Request).
%
%	Add data to the repository

upload_data(Request) :- !,
	http_parameters(Request,
			[ repository(Repository),
			  data(Data, []),
			  dataFormat(DataFormat),
			  baseURI(BaseURI),
			  verifyData(_Verify),
			  resultFormat(ResultFormat)
			],
			[ attribute_declarations(attribute_decl)
			]),
	authorized(write(Repository, load(posted))),
	atom_to_memory_file(Data, MemFile),
	open_memory_file(MemFile, read, Stream),
	action(Request, call_cleanup(load_triples(stream(Stream),
					 [ base_uri(BaseURI),
					   data_format(DataFormat)
					 ]),
			    (	 close(Stream),
				 free_memory_file(MemFile)
			    )),
	       ResultFormat,
	       'Loaded data from POST'-[]).


%%	upload_file(+Request)
%
%	Add data to the repository from a file form

upload_file(Request) :-
	http_parameters(Request,
			[ repository(Repository),
			  fileData(Data, []),
			  dataFormat(DataFormat),
			  baseURI(BaseURI),
			  verifyData(_Verify),
			  resultFormat(ResultFormat)
			],
			[ attribute_declarations(attribute_decl)
			]),
	authorized( write(Repository, load(posted))),
	atom_to_memory_file(Data, MemFile),
	open_memory_file(MemFile, read, Stream),
	action(Request, call_cleanup(load_triples(stream(Stream),
					 [ base_uri(BaseURI),
					   data_format(DataFormat)
					 ]),
			    (	 close(Stream),
				 free_memory_file(MemFile)
			    )),
	       ResultFormat,
	       'Loaded data from POST'-[]).


%%	upload_url(+Request)
%
%	Add data to the repository

upload_url(Request) :-
	http_parameters(Request,
			[ repository(Repository),
			  url(URL, []),
			  dataFormat(DataFormat),
			  baseURI(BaseURI,
				  [ default(URL)
				  ]),
			  verifyData(_Verify),
			  resultFormat(ResultFormat)
			],
			[ attribute_declarations(attribute_decl)
			]),
	authorized(write(Repository, load(url(URL)))),
	http_open(URL, Stream,
		  [ timeout(60)
		  ]),
	action(Request, call_cleanup(load_triples(stream(Stream),
					 [ base_uri(BaseURI),
					   data_format(DataFormat)
					 ]),
			    (	 close(Stream)
			    )),
	       ResultFormat,
	       'Loaded data from ~w'-[URL]).

%%	remove_statements(+Request)
%
%	Remove statements from the database

remove_statements(Request) :-
	http_parameters(Request,
			[ repository(Repository, [optional(true)]),
			  resultFormat(ResultFormat),
					% as documented
			  subject(Subject, [optional(true)]),
			  predicate(Predicate, [optional(true)]),
			  object(Object, [optional(true)]),
					% remove (turtle) graph
			  baseURI(BaseURI),
			  dataFormat(DataFormat),
			  data(Data, [optional(true)])
			],
			[ attribute_declarations(attribute_decl)
			]),
	instantiated(Subject, SI),
	instantiated(Predicate, PI),
	instantiated(Object, OI),
	authorized(write(Repository, remove_statements(SI, PI, OI))),

	(   nonvar(Data)
	->  atom_to_memory_file(Data, MemFile),
	    open_memory_file(MemFile, read, Stream),
	    call_cleanup(get_triples(stream(Stream),
				     Triples,
				     [ base_uri(BaseURI),
				       data_format(DataFormat)
				     ]),
			 (   close(Stream),
			     free_memory_file(MemFile)
			 )),
	    length(Triples, NTriples),
	    debug(removeStatements, 'Removing ~D statements', [NTriples]),
	    action(Request, remove_triples(Triples),
		   ResultFormat,
		   'Removed ~D triples'-[NTriples])
	;   debug(removeStatements, 'removeStatements = ~w',
		  [rdf(Subject, Predicate, Object)]),

	    ntriple_part(Subject,   subject,   S),
	    ntriple_part(Predicate, predicate, P),
	    ntriple_part(Object,    object,    O),

	    debug(removeStatements, 'Action = ~q', [rdf_retractall(S,P,O)]),
	    action(Request, rdf_retractall(S,P,O),
		   ResultFormat,
		   'Removed statements from ~p'-[rdf(S,P,O)])
	).

instantiated(X, I) :-
	(   var(X)
	->  I = (-)
	;   I = (+)
	).

ntriple_part(In, _, _) :-
	var(In), !.
ntriple_part('', _, _) :- !.
ntriple_part(In, Field, Out) :-
	atom_codes(In, Codes),
	phrase(rdf_ntriple_part(Field, Out), Codes), !.
ntriple_part(Text, Field, _) :-
	throw(error(type_error(ntriples(Field), Text),
		    context(_,
			    'Field must be in N-triples notation'))).


%%	attribute_decl(+OptionName, -Options)
%
%	Default   options   for   specified     attribute   names.   See
%	http_parameters/3.

attribute_decl(repository,
	       [ optional(true)
	       ]).
attribute_decl(query,
	       [
	       ]).
attribute_decl(queryLanguage,
	       [ default('SeRQL'),
		 type(oneof(['SeRQL', 'SPARQL']))
	       ]).
attribute_decl(serialization,
	       [ default(rdfxml),
		 type(oneof([ rdfxml,
			      ntriples,
			      n3
			    ]))
	       ]).
attribute_decl(resultFormat,
	       [ default(xml),
		 type(oneof([ xml,
			      html,
			      rdf
			    ]))
	       ]).
attribute_decl(resourceFormat,
	       [ default(ns),
		 type(oneof([ plain,
			      ns,
			      nslabel
			    ]))
	       ]).
attribute_decl(entailment,		% cache?
	       [ default(Default),
		 type(oneof([Es]))
	       ]) :-
	setting(serql_parms:default_entailment, Default),
	findall(E, serql:entailment(E, _), Es).
attribute_decl(dataFormat,
	       [ default(rdfxml),
		 type(oneof([rdfxml, ntriples]))
	       ]).
attribute_decl(baseURI,
	       [ default('foo:bar')
	       ]).
attribute_decl(verifyData, Options)   :- bool(off, Options).
attribute_decl(schema, Options)	      :- bool(off, Options).
attribute_decl(data, Options)	      :- bool(off, Options).
attribute_decl(explicitOnly, Options) :- bool(off, Options).
attribute_decl(niceOutput, Options)   :- bool(off, Options).
attribute_decl(niceOutput, Options)   :- bool(off, Options).
					% Our extensions
attribute_decl(storeAs, [default('')]).

bool(Def,
     [ default(Def),
       type(oneof([on, off]))
     ]).


%%	action(+Request, :Goal, +Format, +Message)
%
%	Perform some -modifying-  goal,  reporting   time,  triples  and
%	subject statistics.

action(_Request, G, Format, Message) :-
	logged_on(User, anonymous),
	get_time(T0), T is integer(T0),
	statistics(cputime, CPU0),
	rdf_statistics(triples(Triples0)),
	rdf_statistics(subjects(Subjects0)),
	run(G, serql(User, T)),
	rdf_statistics(subjects(Subjects1)),
	rdf_statistics(triples(Triples1)),
	statistics(cputime, CPU1),
	CPU is CPU1 - CPU0,
	Triples is Triples1 - Triples0,
	Subjects is Subjects1 - Subjects0,
	done(Format, Message, CPU, Subjects, Triples).

run((A,B), Log) :- !,
	run(A, Log),
	run(B, Log).
run(A, Log) :-
	rdf_transaction(A, Log).


done(html, Fmt-Args, CPU, Subjects, Triples) :-
	format(string(Message), Fmt, Args),
	rdf_statistics(triples(TriplesNow)),
	rdf_statistics(subjects(SubjectsNow)),
	reply_page('Success',
		   [ h4('Operation completed'),
		     p(Message),
		     h4('Statistics'),
		     table([ border(1),
			     cellpadding(3)
			   ],
			   [ tr([td(''), th('+/-'), th('now')]),
			     tr([th(align(right), 'CPU time'),
				 \right(CPU), td('')]),
			     tr([th(align(right), 'Subjects'),
				 \right(Subjects), \right(SubjectsNow)]),
			     tr([th(align(right), 'Triples'),
				 \right(Triples), \right(TriplesNow)])
			   ])
		   ]).
done(xml, Fmt-Args, _CPU, _Subjects, _Triples) :-
	format(string(Message), Fmt, Args),
	format('Content-type: text/xml~n~n'),
	format('<transaction>~n'),
	format('  <status>~n'),
	format('     <msg>~w</msg>~n', [Message]),
	format('  </status>~n'),
	format('</transaction>~n').
done(rdf, Fmt-Args, _CPU, _Subjects, _Triples) :-
	format('Content-type: text/plain~n~n'),
	format('resultFormat=~w not yet supported~n~n'),
	format(Fmt, Args).


right(V) -->
	html(td(align(right), V)).


		 /*******************************
		 *	   SAVED QUERIES	*
		 *******************************/

:- dynamic
	stored_query/4.			% Name, User, Type, Query

store_query(_, '', _) :- !.
store_query(Type, As, Query) :-
	logged_on(User),
	retractall(stored_query(As, User, Type, _)),
	assert(stored_query(As, User, Type, Query)).


		 /*******************************
		 *		EMIT		*
		 *******************************/

reply_page(Title, Content) :-
	phrase(page(title(Title), Content), HTML),
	format('Content-type: text/html~n~n'),
	print_html(HTML).
