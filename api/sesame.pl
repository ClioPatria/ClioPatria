/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2015, University of Amsterdam,
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
:- use_module(rdfql(rdf_io)).
:- use_module(rdfql(rdf_html)).
:- use_module(library(http/http_parameters)).
:- use_module(user(user_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_file_type)).
:- use_module(library(semweb/rdf_persistency)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_request_value)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(memfile)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(settings)).
:- use_module(components(query)).
:- use_module(components(basics)).
:- use_module(components(messages)).

:- meta_predicate(api_action2(+,0,+,+)).

:- http_handler(sesame('login'),	      http_login,	    []).
:- http_handler(sesame('logout'),	      http_logout,	    []).
:- http_handler(sesame('evaluateQuery'),      evaluate_query,
		[spawn(sparql_query)]).
:- http_handler(sesame('evaluateGraphQuery'), evaluate_graph_query,
		[spawn(sparql_query)]).
:- http_handler(sesame('evaluateTableQuery'), evaluate_table_query,
		[spawn(sparql_query)]).
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
:- http_handler(sesame('flushJournal'),	      flush_journal,
		[ time_limit(infinite) ]).
:- http_handler(sesame('modifyPersistency'),  modify_persistency,
		[ time_limit(infinite) ]).

:- html_meta
	api_action(+, 0, +, html).

%%	http_login(+Request)
%
%	HTTP handler to associate the current session with a local user.
%	If the login succeeds a 200  reply according to the resultFormat
%	parameters  is  sent.  If  the  result  fails  due  to  a  wrong
%	user/password,  the  server  responds  with  a  403  (forbidden)
%	message.  Other failures result in a 500 (server error).
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
	result_format(Request, ResultFormat),
	api_action(Request,
		   (   validate_login(Request, User, Password),
		       login(User)
		   ),
		   ResultFormat,
		   'Login ~w'-[User]).

validate_login(_, User, Password) :-
	validate_password(User, Password), !.
validate_login(Request, _, _) :-
	memberchk(path(Path), Request),
	throw(http_reply(forbidden(Path))).


%%      http_logout(+Request)
%
%       HTTP handler to logout current user.

http_logout(Request) :-
	http_parameters(Request,
			[ resultFormat(ResultFormat)
			],
			[ attribute_declarations(attribute_decl)
			]),
	result_format(Request, ResultFormat),
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
	result_format(Request, ResultFormat),
	statistics(cputime, CPU0),
	downcase_atom(QueryLanguage, QLang),
	compile(QLang, Query, Compiled,
		[ entailment(Entailment),
		  type(Type)
		]),
	authorized_query(Type, Repository, ResultFormat),
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
	;   Type == update, Result = [Reply]
	->  reply_html_page(cliopatria(default),
			    title('Update Result'),
			    [ h4('Update query completed'),
			      p(['Answer = ', Reply])
			    ])
	).


authorized_query(update, Repository, ResultFormat) :- !,
	authorized_api(write(Repository, sparql(update)), ResultFormat).
authorized_query(_, Repository, ResultFormat) :-
	authorized_api(read(Repository, query), ResultFormat).

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
	result_format(Request, ResultFormat),
	authorized_api(read(Repository, query), ResultFormat),
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
	result_format(Request, ResultFormat),
	authorized_api(read(Repository, query), ResultFormat),
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
			  resultFormat(ResultFormat)
			],
			[ attribute_declarations(attribute_decl)
			]),
	result_format(Request, ResultFormat),
	authorized_api(write(Repository, clear), ResultFormat),
	api_action(Request,
		   rdf_reset_db,
		   ResultFormat,
		   'Clear database'-[]).

%%	unload_source(+Request)
%
%	Remove triples loaded from a specified source

unload_source(Request) :-
	http_parameters(Request,
			[ repository(Repository),
			  source(Source),
			  resultFormat(ResultFormat)
			],
			[ attribute_declarations(attribute_decl)
			]),
	result_format(Request, ResultFormat),
	authorized_api(write(Repository, unload(Source)), ResultFormat),
	api_action(Request, rdf_unload(Source),
		   ResultFormat,
		   'Unload triples from ~w'-[Source]).


%%	unload_graph(+Request)
%
%	Remove a named graph.

unload_graph(Request) :-
	http_parameters(Request,
			[ repository(Repository),
			  graph(Graph, []),
			  resultFormat(ResultFormat)
			],
			[ attribute_declarations(attribute_decl)
			]),
	result_format(Request, ResultFormat),
	authorized_api(write(Repository, unload(Graph)), ResultFormat),
	api_action(Request, rdf_unload_graph(Graph),
		   ResultFormat,
		   'Unload triples from ~w'-[Graph]).


%%	flush_journal(+Request)
%
%	Flush the journal of the requested graph

flush_journal(Request) :-
	http_parameters(Request,
			[ repository(Repository),
			  graph(Graph, []),
			  resultFormat(ResultFormat)
			],
			[ attribute_declarations(attribute_decl)
			]),
	result_format(Request, ResultFormat),
	authorized_api(write(Repository, unload(Graph)), ResultFormat),
	api_action(Request, rdf_flush_journals([graph(Graph)]),
		   ResultFormat,
		   'Flushed journals for graph ~w'-[Graph]).


%%	modify_persistency(+Request)
%
%	Change the persistent properties for the requested graph

modify_persistency(Request) :-
	http_parameters(Request,
			[ repository(Repository),
			  graph(Graph, []),
			  resultFormat(ResultFormat),
			  persistent(Persistent)
			],
			[ attribute_declarations(attribute_decl)
			]),
	persistency(Persistent, PVal, Action),
	result_format(Request, ResultFormat),
	authorized_api(write(Repository, persistent(Graph)), ResultFormat),
	api_action(Request, rdf_persistency(Graph, PVal),
		   ResultFormat,
		   '~w persistency for graph ~w'-[Action, Graph]).

persistency(on,  true,  'Set').
persistency(off, false, 'Cleared').


%%	upload_data(Request).
%
%	Sesame compliant method  to  upload   data  to  the  repository,
%	typically used to handle a POST-form   from a web-browser (e.g.,
%	_|Load local file|_ in the ClioPatria  menu). If =dataFormat= is
%	omitted, the format of the data is guessed from the data itself.
%	Currently, this possitively identifies valid RDF/XML and assumes
%	that anything else is Turtle.

:- if(current_predicate(http_convert_parameters/3)).
%%	create_tmp_file(+Stream, -Out, +Options) is det.
%
%	Called  from  library(http/http_multipart_plugin)    to  process
%	uploaded file from a form.
%
%	@arg Stream is the input stream. It signals EOF at the end of
%	the part, but must *not* be closed.
%	@arg Options provides information about the part.  Typically,
%	this contains filename(FileName) and optionally media(Type,
%	MediaParams).

:- public create_tmp_file/3.
create_tmp_file(Stream, file(File, Options), Options) :-
	setup_call_catcher_cleanup(
	    tmp_file_stream(binary, File, Out),
	    copy_stream_data(Stream, Out),
	    Why,
	    cleanup(Why, File, Out)).

cleanup(Why, File, Out) :-
	close(Out),
	(   Why == exit
	->  true
	;   catch(delete_file(File), _, true)
	).

%%	upload_data_file(+Request, +FormData, +TempFile, +FileOptions)
%
%	Load RDF from TempFile with  additional   form  data provided in
%	FormData. Options are the options passed  from the uploaded file
%	and include filename(Name) and optionally media(Type, Params).

upload_data_file(Request, Data, TmpFile, FileOptions) :-
	http_convert_parameters(Data,
				[ repository(Repository),
				  dataFormat(DataFormat),
				  baseURI(BaseURI),
				  verifyData(_Verify),
				  resultFormat(ResultFormat)
				],
				attribute_decl),
	result_format(Request, ResultFormat),
	authorized_api(write(Repository, load(posted)), ResultFormat),
	phrase(load_option(DataFormat, BaseURI), LoadOptions),
	append(LoadOptions, FileOptions, Options),
	api_action(Request,
		   setup_call_cleanup(
		       open(TmpFile, read, Stream),
		       rdf_guess_format_and_load(Stream, Options),
		       close(Stream)),
		   ResultFormat,
		   'Load data from POST'-[]).

upload_option(_=_) :- !.
upload_option(Term) :- functor(Term, _, 1).

upload_data(Request) :-
	option(method(post), Request), !,
	http_read_data(Request, Data,
		       [ on_filename(create_tmp_file)
		       ]),
	(   option(data(file(TmpFile, FileOptions)), Data)
	->  true
	;   existence_error(attribute_declaration, data)
	),
	include(upload_option, FileOptions, Options),
	call_cleanup(upload_data_file(Request, Data, TmpFile, Options),
		     catch(delete_file(TmpFile), _, true)).

:- endif.
upload_data(Request) :-
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
	result_format(Request, ResultFormat),
	authorized_api(write(Repository, load(posted)), ResultFormat),
	phrase(load_option(DataFormat, BaseURI), Options),
	atom_to_memory_file(Data, MemFile),
	api_action(Request,
		   setup_call_cleanup(open_memory_file(MemFile, read, Stream),
				      rdf_guess_format_and_load(Stream, Options),
				      ( close(Stream),
					free_memory_file(MemFile)
				      )),
		   ResultFormat,
		   'Load data from POST'-[]).

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
				  [ default(URL)
				  ]),
			  resultFormat(ResultFormat),
			  verifyData(_Verify),
			  repository(Repository)
			],
			[ attribute_declarations(attribute_decl)
			]),
	result_format(Request, ResultFormat),
	authorized_api(write(Repository, load(url(URL))), ResultFormat),
	phrase(load_option(DataFormat, BaseURI), Options),
	api_action(Request,
		   load_from_url(URL, Options),
		   ResultFormat,
		   'Load data from ~w'-[URL]).

load_from_url(URL, Options) :-
	http_open(URL, In,
		  [ cert_verify_hook(ssl_verify)
		  ]),
	call_cleanup(rdf_guess_format_and_load(In, Options),
		     close(In)).

:- public ssl_verify/5.

%%	ssl_verify(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Error)
%
%	Currently we accept  all  certificates.   We  organise  our  own
%	security using SHA1 signatures, so  we   do  not  care about the
%	source of the data.

ssl_verify(_SSL,
	   _ProblemCertificate, _AllCertificates, _FirstCertificate,
	   _Error).

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
	result_format(Request, ResultFormat),
	instantiated(Subject, SI),
	instantiated(Predicate, PI),
	instantiated(Object, OI),
	authorized_api(write(Repository, remove_statements(SI, PI, OI)),
		       ResultFormat),

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
		       'Remove ~D triples'-[NTriples])
	;   debug(removeStatements, 'removeStatements = ~w',
		  [rdf(Subject, Predicate, Object)]),

	    ntriple_part(Subject,   subject,   S),
	    ntriple_part(Predicate, predicate, P),
	    ntriple_part(Object,    object,    O),

	    debug(removeStatements, 'Action = ~q', [rdf_retractall(S,P,O)]),
	    api_action(Request,
		       rdf_retractall(S,P,O),
		       ResultFormat,
		       'Remove statements from ~k'-[rdf(S,P,O)])
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


%%	rdf_ntriple_part(+Type, -Value)//
%
%	Parse one of the fields of  an   ntriple.  This  is used for the
%	SWI-Prolog Sesame (rdf4j.org) implementation   to  realise
%	/servlets/removeStatements. I do not think   public  use of this
%	predicate should be stimulated.

rdf_ntriple_part(subject, Subject) -->
	subject(Subject).
rdf_ntriple_part(predicate, Predicate) -->
	predicate(Predicate).
rdf_ntriple_part(object, Object) -->
	object(Object).

subject(Subject) -->
	uniref(Subject), !.
subject(Subject) -->
	node_id(Subject).

predicate(Predicate) -->
	uniref(Predicate).

object(Object) -->
	uniref(Object), !.
object(Object) -->
	node_id(Object).
object(Object) -->
	literal(Object).


uniref(URI) -->
	"<",
	escaped_uri_codes(Codes),
	">", !,
	{ atom_codes(URI, Codes)
	}.

node_id(node(Id)) -->			% anonymous nodes
	"_:",
	name_start(C0),
	name_codes(Codes),
	{ atom_codes(Id, [C0|Codes])
	}.

literal(Literal) -->
	lang_string(Literal), !.
literal(Literal) -->
	xml_string(Literal).


%	name_start(-Code)
%	name_codes(-ListfCodes)
%
%	Parse identifier names

name_start(C) -->
	[C],
	{ code_type(C, alpha)
	}.

name_codes([C|T]) -->
	[C],
	{ code_type(C, alnum)
	}, !,
	name_codes(T).
name_codes([]) -->
	[].


%	escaped_uri_codes(-CodeList)
%
%	Decode string holding %xx escaped characters.

escaped_uri_codes([]) -->
	[].
escaped_uri_codes([C|T]) -->
	"%", [D0,D1], !,
	{ code_type(D0, xdigit(V0)),
	  code_type(D1, xdigit(V1)),
	  C is V0<<4 + V1
	},
	escaped_uri_codes(T).
escaped_uri_codes([C|T]) -->
	"\\u", [D0,D1,D2,D3], !,
	{ code_type(D0, xdigit(V0)),
	  code_type(D1, xdigit(V1)),
	  code_type(D2, xdigit(V2)),
	  code_type(D3, xdigit(V3)),
	  C is V0<<12 + V1<<8 + V2<<4 + V3
	},
	escaped_uri_codes(T).
escaped_uri_codes([C|T]) -->
	"\\U", [D0,D1,D2,D3,D4,D5,D6,D7], !,
	{ code_type(D0, xdigit(V0)),
	  code_type(D1, xdigit(V1)),
	  code_type(D2, xdigit(V2)),
	  code_type(D3, xdigit(V3)),
	  code_type(D4, xdigit(V4)),
	  code_type(D5, xdigit(V5)),
	  code_type(D6, xdigit(V6)),
	  code_type(D7, xdigit(V7)),
	  C is V0<<28 + V1<<24 + V2<<20 + V3<<16 +
	       V4<<12 + V5<<8 + V6<<4 + V7
	},
	escaped_uri_codes(T).
escaped_uri_codes([C|T]) -->
	[C],
	escaped_uri_codes(T).

%	lang_string()
%
%	Process a language string

lang_string(String) -->
	"\"",
	string(Codes),
	"\"", !,
	{ atom_codes(Atom, Codes)
	},
	(   langsep
	->  language(Lang),
	    { String = literal(lang(Lang, Atom))
	    }
	;   "^^"
	->  uniref(Type),
	    { String = literal(type(Type, Atom))
	    }
	;   { String = literal(Atom)
	    }
	).

langsep -->
	"-".
langsep -->
	"@".

%	xml_string(String)
%
%	Handle xml"..."

xml_string(xml(String)) -->
	"xml\"",			% really no whitespace?
	string(Codes),
	"\"",
	{ atom_codes(String, Codes)
	}.

string([]) -->
	[].
string([C0|T]) -->
	string_char(C0),
	string(T).

string_char(0'\\) -->
	"\\\\".
string_char(0'") -->
	"\\\"".
string_char(10) -->
	"\\n".
string_char(13) -->
	"\\r".
string_char(9) -->
	"\\t".
string_char(C) -->
	"\\u",
	'4xdigits'(C).
string_char(C) -->
	"\\U",
	'4xdigits'(C0),
	'4xdigits'(C1),
	{ C is C0<<16 + C1
	}.
string_char(C) -->
	[C].

'4xdigits'(C) -->
	[C0,C1,C2,C3],
	{ code_type(C0, xdigit(V0)),
	  code_type(C1, xdigit(V1)),
	  code_type(C2, xdigit(V2)),
	  code_type(C3, xdigit(V3)),

	  C is V0<<12 + V1<<8 + V2<<4 + V3
	}.

%	language(-Lang)
%
%	Return xml:lang language identifier.

language(Lang) -->
	lang_code(C0),
	lang_codes(Codes),
	{ atom_codes(Lang, [C0|Codes])
	}.

lang_code(C) -->
	[C],
	{ C \== 0'.,
	  \+ code_type(C, white)
	}.

lang_codes([C|T]) -->
	lang_code(C), !,
	lang_codes(T).
lang_codes([]) -->
	[].



		 /*******************************
		 *	 HTTP ATTRIBUTES	*
		 *******************************/

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
	       [ optional(true),
		 oneof([ xml,
			 html,
			 rdf,
			 json,
			 csv
		       ]),
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
attribute_decl(persistent,
	       [ description('Modify persistency of a graph'),
		 oneof([on, off])
	       ]).

bool(Def,
     [ default(Def),
       oneof([on, off])
     ]).


%%	result_format(+Request, ?Format) is det.

result_format(_Request, Format) :-
	atom(Format), !.
result_format(Request, _Format) :-
	memberchk(accept(Accept), Request),
	debug(sparql(result), 'Got accept = ~q', [Accept]),
	fail.
result_format(_Request, xml).


accept_output_format(Request, Format) :-
	memberchk(accept(Accept), Request),
	(   atom(Accept)
	->  http_parse_header_value(accept, Accept, Media)
	;   Media = Accept
	),
	find_media(Media, Format), !.
accept_output_format(_, xml).

find_media([media(Type, _, _, _)|T], Format) :-
	(   sparql_media(Type, Format)
	->  true
	;   find_media(T, Format)
	).

sparql_media(application/'sparql-results+xml',   xml).
sparql_media(application/'sparql-results+json', json).

%%	api_action(+Request, :Goal, +Format, +Message)
%
%	Perform some -modifying-  goal,  reporting   time,  triples  and
%	subject statistics.
%
%	@param	Format specifies the result format and is one of =html=,
%		=xml= or =rdf=.
%	@param	Message is passed to html_write//1.

api_action(Request, G, html, Message) :- !,
	call_showing_messages(
	    api_action2(Request, G, html, Message),
	    [ header(h4(Message)),
	      footer([])
	    ]).
api_action(Request, G, Format, Message) :-
	api_action2(Request, G, Format, Message).

api_action2(_Request, G, Format, Message) :-
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

:- meta_predicate
	run(0, +).

run(M:(A,B), Log) :- !,
	run(M:A, Log),
	run(M:B, Log).
run(Goal, _) :-
	no_transaction(Goal), !,
	call(Goal).
run(A, Log) :-
	rdf_transaction(A, Log).

no_transaction(_:rdf_reset_db).
no_transaction(_:rdf_unload_graph(_)).
no_transaction(_:rdf_flush_journals(_)).

done(html, _Message, CPU, Subjects, Triples) :-
	after_messages([ \result_table(CPU, Subjects, Triples)
		       ]).
done(Format, _:Message, CPU, Subjects, Triples) :- !,
	done(Format, Message, CPU, Subjects, Triples).
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
	format('resultFormat=rdf not yet supported~n~n'),
	format(Fmt, Args).


%%	result_table(+CPU, +SubDiff, +TripleDiff)// is det.
%
%	HTML component that summarises the result of an operation.

result_table(CPU, Subjects, Triples) -->
	{ rdf_statistics(triples(TriplesNow)),
	  subjects(SubjectsNow)
	},
	html([ h4('Operation completed'),
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


%%	authorized_api(+Action, +ResultFormat) is det.
%
%	@error permission_error(http_location, access, Path)

authorized_api(Action, ResultFormat) :-
	ResultFormat == html, !,	% do not bind
	authorized(Action).
authorized_api(Action, _) :-
	logged_on(User, anonymous),
	check_permission(User, Action).
