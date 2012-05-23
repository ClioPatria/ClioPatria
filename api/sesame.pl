/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2010, University of Amsterdam,
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

:- module(api_sesame,
	  [ api_action/4		% +Request, +Goal, +Format, +Message
	  ]).
:- use_module(rdfql(serql)).
:- use_module(rdfql(sparql)).
:- use_module(rdfql(serql_xml_result)).
:- use_module(rdfql(rdf_io)).
:- use_module(rdfql(rdf_html)).
:- use_module(library(http/http_parameters)).
:- use_module(user(user_db)).
:- use_module(library(semweb/rdf_edit)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_file_type)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(memfile)).
:- use_module(library(rdf_ntriples)).
:- use_module(library(debug)).
:- use_module(library(settings)).
:- use_module(components(query)).
:- use_module(components(basics)).

:- http_handler(sesame('login'),	      http_login,	    []).
:- http_handler(sesame('logout'),	      http_logout,	    []).
:- http_handler(sesame('evaluateQuery'),      evaluate_query,	    [spawn(sparql_query)]).
:- http_handler(sesame('evaluateGraphQuery'), evaluate_graph_query, [spawn(sparql_query)]).
:- http_handler(sesame('evaluateTableQuery'), evaluate_table_query, [spawn(sparql_query)]).
:- http_handler(sesame('extractRDF'),	      extract_rdf,	    []).
:- http_handler(sesame('listRepositories'),   list_repositories,    []).
:- http_handler(sesame('clearRepository'),    clear_repository,	    []).
:- http_handler(sesame('unloadSource'),	      unload_source,
		[ time_limit(infinite) ]).
:- http_handler(sesame('unloadGraph'),	      unload_graph,
		[ time_limit(infinite) ]).
:- http_handler(sesame('uploadData'),	      upload_data,
		[ time_limit(infinite) ]).
:- http_handler(sesame('uploadURL'),	      upload_url,
		[ time_limit(infinite) ]).
:- http_handler(sesame('removeStatements'),   remove_statements,
		[ time_limit(infinite) ]).

:- html_meta
	api_action(+, 0, +, html).

%%	http_login(+Request)
%
%	HTTP handler to associate the current session with a local user.
%
%	@see	help('howto/ClientAuth.txt') for additional information on
%		authetication.

http_login(Request) :-
	http_parameters(Request,
			[ user(User),
			  password(Password),
			  resultFormat(ResultFormat)
			],
			[ attribute_declarations(attribute_decl)
			]),
	api_action(Request,
		   (   validate_password(User, Password),
		       login(User)
		   ),
		   ResultFormat,
		   'Login ~w'-[User]).

%%      http_logout(+Request)
%
%       HTTP handler to logout current user.

http_logout(Request) :-
	http_parameters(Request,
			[ resultFormat(ResultFormat)
			],
			[ attribute_declarations(attribute_decl)
			]),
	api_action(Request,
		   logout_user(Message),
		   ResultFormat,
		   Message).

logout_user('Logout ~w'-[User]) :-
	logged_on(User), !,
	logout(User).
logout_user('Not logged on'-[]).


%%	evaluate_query(+Request) is det.
%
%	HTTP handler for both SeRQL  and   SPARQL  queries. This handler
%	deals with _interactive_  queries.   Machines  typically  access
%	/sparql/ to submit queries and process   result compliant to the
%	SPARQL protocol.

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
	;   Type == ask, Result = [Reply]
	->  reply_html_page(cliopatria(default),
			    title('ASK Result'),
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
%	HTTP handler to extract RDF  from   the  database.  This handler
%	separates the data into schema data   and non-schema data, where
%	schema data are triples  whose  subject   is  an  rdfs:Class  or
%	rdf:Property. By default both are =off=,   so  one needs to pass
%	either or both of the =schema= and =data= options as =on=.

extract_rdf(Request) :-
	http_parameters(Request,
			[ repository(Repository),
			  schema(Schema),
			  data(Data),
			  explicitOnly(ExplicitOnly),
			  niceOutput(_NiceOutput),
			  serialization(Serialization)
			],
			[ attribute_declarations(attribute_decl)
			]),
	authorized(read(Repository, download)),
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
	rdfs_individual_of(S, rdf:'Property').
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
	api_action(Request,
		   rdf_reset_db,
		   Format,
		   'Cleared database'-[]).

%%	unload_source(+Request)
%
%	Remove triples loaded from a specified source

unload_source(Request) :-
	http_parameters(Request,
			[ repository(Repository),
			  source(Source),
			  resultFormat(Format)
			],
			[ attribute_declarations(attribute_decl)
			]),
	authorized(write(Repository, unload(Source))),
	api_action(Request, rdf_unload(Source),
		   Format,
		   'Unloaded triples from ~w'-[Source]).


%%	unload_graph(+Request)
%
%	Remove a named graph.

unload_graph(Request) :-
	http_parameters(Request,
			[ repository(Repository),
			  graph(Graph, []),
			  resultFormat(Format)
			],
			[ attribute_declarations(attribute_decl)
			]),
	authorized(write(Repository, unload(Graph))),
	api_action(Request, rdf_unload(Graph),
		   Format,
		   'Unloaded triples from ~w'-[Graph]).


%%	upload_data(Request).
%
%	Sesame compliant method  to  upload   data  to  the  repository,
%	typically used to handle a POST-form   from a web-browser (e.g.,
%	_|Load local file|_ in the ClioPatria  menu). If =dataFormat= is
%	omitted, the format of the data is guessed from the data itself.
%	Currently, this possitively identifies valid RDF/XML and assumes
%	that anything else is Turtle.

upload_data(Request) :- !,
	http_parameters(Request,
			[ repository(Repository),
			  data(Data,
			       [ description('RDF data to be loaded')
			       ]),
			  dataFormat(DataFormat),
			  baseURI(BaseURI),
			  verifyData(_Verify),
			  resultFormat(ResultFormat)
			],
			[ attribute_declarations(attribute_decl)
			]),
	authorized(write(Repository, load(posted))),
	phrase(load_option(DataFormat, BaseURI), Options),
	atom_to_memory_file(Data, MemFile),
	api_action(Request,
		   setup_call_cleanup(open_memory_file(MemFile, read, Stream),
				      rdf_guess_format_and_load(Stream, Options),
				      ( close(Stream),
					free_memory_file(MemFile)
				      )),
		   ResultFormat,
		   'Loaded data from POST'-[]).


%%	upload_url(+Request)
%
%	Load data from an HTTP server. This API is compatible to Sesame,
%	although the =verifyData= option  is   not  implemented (data is
%	always checked for syntax). Unlike Sesame, the default format is
%	not =rdfxml=, but derived from the  Content-type reported by the
%	server.
%
%	@see Calls rdf_load/2 for the actual loading.
%	@see load_url_form/1 a form to access this API

upload_url(Request) :-
	http_parameters(Request,
			[ url(URL, []),
			  dataFormat(DataFormat),
			  baseURI(BaseURI,
				  [ optional(true)
				  ]),
			  resultFormat(ResultFormat),
			  verifyData(_Verify),
			  repository(Repository)
			],
			[ attribute_declarations(attribute_decl)
			]),
	authorized(write(Repository, load(url(URL)))),
	phrase(load_option(DataFormat, BaseURI), Options),
	api_action(Request,
		   rdf_load(URL, Options),
		   ResultFormat,
		   'Loaded data from ~w'-[URL]).

load_option(DataFormat, BaseURI) -->
	data_format_option(DataFormat),
	base_uri_option(BaseURI).

data_format_option(Var)      --> {var(Var)}, !.
data_format_option(rdfxml)   --> [format(xml)].
data_format_option(ntriples) --> [format(turtle)].
data_format_option(turtle)   --> [format(turtle)].

base_uri_option(Var) --> {var(Var)}, !.
base_uri_option(URI) --> [base_uri(URI)].


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
	->  setup_call_cleanup(( atom_to_memory_file(Data, MemFile),
				 open_memory_file(MemFile, read, Stream,
						  [ free_on_close(true)
						  ])
			       ),
			       ( rdf_guess_data_format(Stream, DataFormat),
			         get_triples(stream(Stream),
					     Triples,
					     [ base_uri(BaseURI),
					       data_format(DataFormat)
					     ])
			       ),
			       close(Stream)),
	    length(Triples, NTriples),
	    debug(removeStatements, 'Removing ~D statements', [NTriples]),
	    api_action(Request,
		       remove_triples(Triples),
		       ResultFormat,
		       'Removed ~D triples'-[NTriples])
	;   debug(removeStatements, 'removeStatements = ~w',
		  [rdf(Subject, Predicate, Object)]),

	    ntriple_part(Subject,   subject,   S),
	    ntriple_part(Predicate, predicate, P),
	    ntriple_part(Object,    object,    O),

	    debug(removeStatements, 'Action = ~q', [rdf_retractall(S,P,O)]),
	    api_action(Request,
		       rdf_retractall(S,P,O),
		       ResultFormat,
		       'Removed statements from ~p'-[rdf(S,P,O)])
	).

%%	remove_triples(+List)
%
%	Remove indicated triples from the database.

remove_triples([]).
remove_triples([rdf(S,P,O)|T]) :-
	rdf_retractall(S,P,O),
	remove_triples(T).

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
	       [ optional(true),
		 description('Name of the repository (ignored)')
	       ]).
attribute_decl(query,
	       [ description('SPARQL or SeRQL quer-text')
	       ]).
attribute_decl(queryLanguage,
	       [ default('SPARQL'),
		 oneof(['SeRQL', 'SPARQL']),
		 description('Query language used in query-text')
	       ]).
attribute_decl(serialization,
	       [ default(rdfxml),
		 oneof([ rdfxml,
			 ntriples,
			 n3
		       ]),
		 description('Serialization for graph-data')
	       ]).
attribute_decl(resultFormat,
	       [ default(xml),
		 type(oneof([ xml,
			      html,
			      rdf
			    ])),
		 description('Serialization format of the result')
	       ]).
attribute_decl(resourceFormat,
	       [ default(ns),
		 oneof([ plain,
			 ns,
			 nslabel
		       ]),
		 description('How to format URIs in the table')
	       ]).
attribute_decl(entailment,		% cache?
	       [ default(Default),
		 oneof(Es),
		 description('Reasoning performed')
	       ]) :-
	setting(cliopatria:default_entailment, Default),
	findall(E, cliopatria:entailment(E, _), Es).
attribute_decl(dataFormat,
	       [ optional(true),
		 oneof([rdfxml, ntriples, turtle]),
		 description('Serialization of the data')
	       ]).
attribute_decl(baseURI,
	       [ default('http://example.org/'),
		 description('Base URI for relative resources')
	       ]).
attribute_decl(source,
	       [ description('Name of the graph')
	       ]).
attribute_decl(verifyData,
	       [ description('Verify the data (ignored)')
	       | Options
	       ]) :-
	bool(off, Options).
attribute_decl(schema,
	       [ description('Include schema RDF in downloaded graph')
	       | Options
	       ]) :-
	bool(off, Options).
attribute_decl(data,
	       [ description('Include non-schema RDF in downloaded graph')
	       | Options
	       ]) :-
	bool(off, Options).
attribute_decl(explicitOnly,
	       [ description('Do not include entailed triples')
	       | Options
	       ]) :-
	bool(off, Options).
attribute_decl(niceOutput,
	       [ description('Produce human-readable output (ignored; we always do that)')
	       | Options
	       ]) :-
	bool(off, Options).
attribute_decl(user,
	       [ description('User name')
	       ]).
attribute_decl(password,
	       [ description('Clear-text password')
	       ]).

					% Our extensions
attribute_decl(storeAs,
	       [ default(''),
		 description('Store query under this name')
	       ]).

bool(Def,
     [ default(Def),
       oneof([on, off])
     ]).


%%	api_action(+Request, :Goal, +Format, +Message)
%
%	Perform some -modifying-  goal,  reporting   time,  triples  and
%	subject statistics.
%
%	@param	Format specifies the result format and is one of =html=,
%		=xml= or =rdf=.
%	@param	Message is passed to html_write//1.

api_action(_Request, G, Format, Message) :-
	logged_on(User, anonymous),
	get_time(T0), T is integer(T0),
	statistics(cputime, CPU0),
	rdf_statistics(triples(Triples0)),
	subjects(Subjects0),
	run(G, sesame(User, T)),
	subjects(Subjects1),
	rdf_statistics(triples(Triples1)),
	statistics(cputime, CPU1),
	CPU is CPU1 - CPU0,
	Triples is Triples1 - Triples0,
	Subjects is Subjects1 - Subjects0,
	done(Format, Message, CPU, Subjects, Triples).

:- if(rdf_statistics(subjects(_))).	% RDF 2.x
subjects(Count) :- rdf_statistics(subjects(Count)).
subj_label --> html('Subjects').
:- else.				% RDF 3.0
subjects(Count) :- rdf_statistics(resources(Count)).
subj_label --> html('Resources').
:- endif.


run((A,B), Log) :- !,
	run(A, Log),
	run(B, Log).
run(A, Log) :-
	rdf_transaction(A, Log).


done(html, Message, CPU, Subjects, Triples) :-
	reply_html_page(cliopatria(default),
			title('Success'),
			\result_table(Message, CPU, Subjects, Triples)).
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


%%	result_table(+Message, +CPU, +SubDiff, +TripleDiff)// is det.
%
%	HTML component that summarises the result of an operation.

result_table(Message, CPU, Subjects, Triples) -->
	{ rdf_statistics(triples(TriplesNow)),
	  subjects(SubjectsNow)
	},
	html([ h4('Operation completed'),
	       p(class(msg_informational), Message),
	       h4('Statistics'),
	       table([ id('result'),
		       class(block)
		     ],
		     [ tr([td(class(empty), ''), th('+/-'), th('now')]),
		       tr([th(class(p_name), 'CPU time'),
			   \nc('~3f', CPU), td('')]),
		       tr([th(class(p_name), \subj_label),
			   \nc('~D', Subjects), \nc('~D', SubjectsNow)]),
		       tr([th(class(p_name), 'Triples'),
			   \nc('~D', Triples), \nc('~D', TriplesNow)])
		     ])
	     ]).
