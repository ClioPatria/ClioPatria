/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Maarten Menken & Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, VU Amsterdam, University of Amsterdam

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

:- module(sesame_client,
	  [ sesame_login/3,		% +User, +Password, +Options
	    sesame_logout/1,		% +Options

	    sesame_current_repository/3,% -Id, -Attributes, +Options
	    sesame_clear_repository/1,	% +Options

	    sesame_graph_query/3,	% +Query, -RDF, +Options
	    sesame_table_query/3,	% +Query, -Row, +Options
	    sesame_extract_rdf/2,	% -RDF, +Options

	    sesame_upload_file/2,	% +File, +Options
	    sesame_assert/2,		% +Triples, +Options
	    sesame_retract/2,		% +Triple, +Options

	    set_sesame_default/1	% +Option
	  ]).
:- use_module(library(rdf)).
:- use_module(library('http/http_open')).
:- use_module(library('http/http_client')).
:- use_module(xml_result).
:- use_module(library(rdf_write)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(socket)).
:- use_module(library(debug)).


		 /*******************************
		 *	   REPOSITORIES		*
		 *******************************/

%%	sesame_current_repository(-Id, -Attributes, +Options)
%	
%	Retrieve the repositories from a Sesame server with their
%	attributes.

sesame_current_repository(Id,
			  [ title(Title),
			    readable(Read),
			    writeable(Write)
			  ], Options) :-
	sesame_param(host(Host), Options),
	sesame_param(port(Port), Options),
	sesame_param(path(Path), Options),

	cookie_options(Host, Port, Path, Cookie),
	atom_concat(Path, '/servlets/listRepositories', ListPath),
	
	http_open([ protocol(http),
		    host(Host),
		    port(Port),
		    path(ListPath)
		  ], Sesame,
		  Cookie),

	load_structure(stream(Sesame),
		       [ element(repositorylist, _, Repositories) ],
		       [ dialect(xml),
			 space(remove)
		       ]),
	member(element(repository, Atts, Content), Repositories),
	xml_req_attribute(Atts, id, Id),
	xml_req_attribute(Atts, readable, Read),
	xml_req_attribute(Atts, writeable, Write),
	memberchk(element(title, _, [Title]), Content).
	

xml_req_attribute(Atts, Name, Value) :-
	memberchk(Name=Value, Atts), !.
xml_req_attribute(_, Name, _) :-
	throw(error(existence_error(attribute, Name), _)).


%%	sesame_clear_repository(+Options)
%	
%	Remove all triples from the given repository

sesame_clear_repository(Options) :-
	sesame_param(host(Host), Options),
	sesame_param(port(Port), Options),
	sesame_param(path(Path), Options),
	sesame_param(repository(Repository), Options),

	cookie_options(Host, Port, Path, Cookie),
	debug(cookie, 'Adding cookie option: ~w', [Cookie]),
	atom_concat(Path, '/servlets/clearRepository', ActionPath),
	
	http_post([ protocol(http),
		    host(Host),
		    port(Port),
		    path(ActionPath)
		  ],
		  form([ repository=Repository
		       ]),
		  Reply,
		  [ reply_header(ReplyHeader)
		  | Cookie
		  ]),

	ok(ReplyHeader, Reply).

ok(ReplyHeader, _Reply) :-
	memberchk(status(ok, _), ReplyHeader), !.
ok(ReplyHeader, _Reply) :-
	memberchk(status(forbidden, _), ReplyHeader), !,
	throw(error(permission_error(sesame, http, action), _)). % TBD
ok(_, Reply) :-
	format(user_error, 'ERROR: Reply: ~p~n', [Reply]),
	fail.


		 /*******************************
		 *	      LOGIN		*
		 *******************************/

:- dynamic
	cookie/4.			% Host, Port, Path, Cookie

%%	sesame_login(+User, +Password, Options)
%	
%	Perform a login on a remote HTTP Sesame server.

sesame_login(User, Password, Options) :-
	sesame_param(host(Host), Options),
	sesame_param(port(Port), Options),
	sesame_param(path(Path), Options),

	atom_concat(Path, '/servlets/login', LoginPath),

	(   Password == ''
	->  PwdOptions = []
	;   PwdOptions = [ password=Password ]
	),

	http_post([ protocol(http),
		    host(Host),
		    port(Port),
		    path(LoginPath)
		  ],
		  form([ user=User
		       | PwdOptions
		       ]),
		  Reply,		
		  [ reply_header(ReplyHdr)
		  ]),
	memberchk(set_cookie(set_cookie(sesame_sid, Cookie, _Opts)), ReplyHdr),
	retractall(cookie(Host, Port, Path, _)),
	assert(cookie(Host, Port, Path, Cookie)),
	ok(ReplyHdr, Reply).
	
%%	sesame_logout(+Options)
%	
%	Perform a logout from a remote HTTP Sesame server.

sesame_logout(Options) :-
	sesame_param(host(Host), Options),
	sesame_param(port(Port), Options),
	sesame_param(path(Path), Options),

	cookie_options(Host, Port, Path, Cookie),

	atom_concat(Path, '/servlets/logout', LogoutPath),

	http_open([ protocol(http),
		    host(Host),
		    port(Port),
		    path(LogoutPath)
		  ], Sesame,
		  Cookie),
	close(Sesame),
	retract(cookie(Host, Port, Path, _)).

cookie_options(Host, Port, Path,
	       [ request_header('Cookie'=Cookie)
	       ]) :-
	cookie(Host, Port, Path, CookieValue), !,
	sformat(Cookie, 'sesame_sid=~w', [CookieValue]).
cookie_options(_, _, _, []).



		 /*******************************
		 *	      QUERY		*
		 *******************************/

%%	sesame_graph_query(+Query, rdf(-Subj, -Pred, -Obj), +Options)
%	
%	Run query on a remote  SeRQL   server  using  the HTTP protocol,
%	returning  the  triples  of  the    result-graph  one-by-one  on
%	backtracking

sesame_graph_query(Query, rdf(Subject, Predicate, Object), Options) :-
	sesame_param(host(Host), Options),
	sesame_param(port(Port), Options),
	sesame_param(path(Path), Options),
	sesame_param(repository(Repository), Options),

	cookie_options(Host, Port, Path, Cookie),
	atom_concat(Path, '/servlets/evaluateGraphQuery', GraphQueryPath),
	SearchVars = [ repository = Repository,
		       query = Query,
		       queryLanguage = 'SeRQL'
		     ],

	concat_atom([ 'http', '://', Host, ':', Port,
		      GraphQueryPath,
		      '?', 'repository=', Repository,
		      '&', 'query=', Query
		    ], BaseUri),

	http_open([ protocol(http),
		    host(Host),
		    port(Port),
		    path(GraphQueryPath),
		    search(SearchVars)
		  ], Sesame, Cookie),

	call_cleanup(load_rdf(Sesame, Triples, [base_uri(BaseUri)]),
		     close(Sesame)),

	member(rdf(Subject,Predicate,Object), Triples).


%%	sesame_table_query(+Query, -Row, +Options)
%	
%	Run query on a remote  SeRQL   server  using  the HTTP protocol,
%	returning  the  rows  of   the    result-table   one-by-one   on
%	backtracking

sesame_table_query(Query, Row, Options) :-
	sesame_param(host(Host), Options),
	sesame_param(port(Port), Options),
	sesame_param(path(Path), Options),
	sesame_param(repository(Repository), Options),

	cookie_options(Host, Port, Path, Cookie),
	atom_concat(Path, '/servlets/evaluateTableQuery', ActionPath),
	SearchVars = [ repository = Repository,
		       query = Query,
		       queryLanguage = 'SeRQL'
		     ],

	http_open([ protocol(http),
		    host(Host),
		    port(Port),
		    path(ActionPath),
		    search(SearchVars)
		  ], Sesame, Cookie),

	(   debugging(sesame_reply(File))
	->  open(File, write, Out),
	    copy_stream_data(Sesame, Out),
	    close(Sesame)
	;   call_cleanup(xml_read_result_table(Sesame, Rows, VarNames),
			 close(Sesame)),
	    
	    (   memberchk(variables(VarNames),  Options)
	    ->  true
	    ;   true
	    ),

	    member(Row, Rows)
	).


%%	sesame_extract_rdf(-Triple, +Options)
%	
%	Extract all contents from a remote Sesame repository,  In
%	addition to the server location, the following options are
%	provided:
%	
%%		schema(OnOff)		Extract schema data [on]
%%		data(OnOff)		Extract RDF data [on]
%%		explicitOnly(OnOff)	Use entailment rules?

sesame_extract_rdf(Triple, Options) :-
	sesame_param(host(Host), Options),
	sesame_param(port(Port), Options),
	sesame_param(path(Path), Options),
	sesame_param(repository(Repository), Options),

	option(schema(IncludeSchema), Options, on),
	option(data(IncludeData), Options, on),
	option(explicit_only(Explicitonly), Options, off),

	cookie_options(Host, Port, Path, Cookie),
	atom_concat(Path, '/servlets/extractRDF', ExtractPath),
	SearchVars = [ repository = Repository,
		       schema = IncludeSchema,
		       data = IncludeData,
		       explicitOnly = Explicitonly
		     ],

	concat_atom([ 'http', '://', Host, ':', Port,
		      ExtractPath,
		      '?', 'repository=', Repository
		    ], BaseUri),

	http_open([ protocol(http),
		    host(Host),
		    port(Port),
		    path(ExtractPath),
		    search(SearchVars)
		  ], Sesame,
		  Cookie),

	call_cleanup(load_rdf(Sesame, Triples, [base_uri(BaseUri)]),
		     close(Sesame)),

	member(Triple, Triples).


		 /*******************************
		 *	      UPLOAD		*
		 *******************************/

sesame_upload_file(File, Options) :-
	sesame_param(host(Host), Options),
	sesame_param(port(Port), Options),
	sesame_param(path(Path), Options),
	sesame_param(repository(Repository), Options),
	
	option(data_format(DataFormat), Options, rdfxml),
	option(base_uri(BaseURI), Options, _),
	option(verify_data(Verify), Options, off),
	
	(   var(BaseURI)
	->  absolute_file_name(File, AbsFile),
	    gethostname(MyHost),
	    concat_atom(['file:/', MyHost, '/', AbsFile], BaseURI)
	;   true
	),

	cookie_options(Host, Port, Path, Cookie),
	atom_concat(Path, '/servlets/uploadFile', ActionPath),
	
	http_post([ protocol(http),
		    host(Host),
		    port(Port),
		    path(ActionPath)
		  ],
		  form_data([ repository   = Repository,
			      fileData     = file(File),
			      dataFormat   = DataFormat,
			      baseURI      = BaseURI,
			      verifyData   = Verify
			    ]),
		  Reply,
		  [ reply_header(ReplyHdr)
		  | Cookie
		  ]),
	ok(ReplyHdr, Reply).


%%	sesame_assert(+TripleOrList, +Options)
%	
%	Add a triple or list of triples to the server.

sesame_assert(Var, _) :-
	var(Var),
	throw(error(instantiation_error, _)).
sesame_assert(rdf(S,P,O), Options) :- !,
	sesame_assert([rdf(S,P,O)], Options).
sesame_assert(Triples, Options) :-
	sesame_param(host(Host), Options),
	sesame_param(port(Port), Options),
	sesame_param(path(Path), Options),
	sesame_param(repository(Repository), Options),
	
	option(base_uri(BaseURI), Options, 'foo:bar'),

	cookie_options(Host, Port, Path, Cookie),
	atom_concat(Path, '/servlets/uploadData', ActionPath),
	
	new_memory_file(X),
	open_memory_file(X, write, Out),
	rdf_write_xml(Out, Triples),
	close(Out),
	memory_file_to_atom(X, Data),

	http_post([ protocol(http),
		    host(Host),
		    port(Port),
		    path(ActionPath)
		  ],
		  form([ repository   = Repository,
			 data         = Data,
			 dataFormat   = rdfxml,
			 baseURI      = BaseURI,
			 verifyData   = off,
			 resultFormat = xml
		       ]),
		  Reply,
		  [ reply_header(ReplyHdr)
		  | Cookie
		  ]),
	ok(ReplyHdr, Reply).


%%	sesame_retract(+Triple, +Options)
%	
%	Retract facts from a sesame server

sesame_retract(Triple, Options) :-
	sesame_param(host(Host), Options),
	sesame_param(port(Port), Options),
	sesame_param(path(Path), Options),
	sesame_param(repository(Repository), Options),

	cookie_options(Host, Port, Path, Cookie),
	atom_concat(Path, '/servlets/removeStatements', ActionPath),
	phrase(remove_options(Triple), TripleOptions),

	http_post([ protocol(http),
		    host(Host),
		    port(Port),
		    path(ActionPath)
		  ],
		  form([ repository   = Repository,
			 resultFormat = xml
		       | TripleOptions
		       ]),
		  Reply,
		  [ reply_header(ReplyHdr)
		  | Cookie
		  ]),
	ok(ReplyHdr, Reply).

remove_options(rdf(S,P,O)) -->
	remove_option(S, subject),
	remove_option(P, predicate),
	remove_option(O, object).

remove_option(X, _) -->
	{ var(X) }, !,
	[].
remove_option(X, Field) -->
	{ ntriple_encode(X, Encoded)
	},
	[ Field = Encoded
	].

%	VERY simple minded encoding.  Need to do escapes!!

ntriple_encode(Atom, Encoded) :-
	atom(Atom), !,
	concat_atom(['<', Atom, '>'], Encoded).
ntriple_encode(literal(lang(Lang, String)), Encoded) :- !,
	concat_atom(['"', String, '"@', Lang], Encoded).
ntriple_encode(literal(type(Type, String)), Encoded) :- !,
	concat_atom(['"', String, '"^^<', Type, '>'], Encoded).
ntriple_encode(literal(String), Encoded) :-
	concat_atom(['"', String, '"'], Encoded).


		 /*******************************
		 *	      SETTINGS		*
		 *******************************/

:- dynamic
	sesame_setting/1.

sesame_param(Param, Options) :-
	memberchk(Param, Options), !.
sesame_param(Param, _Options) :-
	sesame_setting(Param), !.
sesame_param(Param, _Options) :-
	functor(Param, Name, _),
	throw(error(existence_error(option, Name), _)).

%%	set_sesame_default(+OptionOrList)
%	
%	Set sesame server default options.  Provided defaults are:
%	host, port and repository.  For example:
%	
%		set_sesame_default([ host(localhost),
%				     port(8080)
%				     repository(world)
%				   ])

set_sesame_default([]) :- !.
set_sesame_default([H|T]) :- !,
	set_sesame_default(H),
	set_sesame_default(T).
set_sesame_default(Term) :-
	functor(Term, Name, Arity),
	functor(Unbound, Name, Arity),
	retractall(sesame_setting(Unbound)),
	assert(sesame_setting(Term)).

