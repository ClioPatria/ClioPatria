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

:- module(http_user, []).

:- use_module(server).
:- use_module(xml_result).
:- use_module(library('http/http_open')).
:- use_module(library('http/thread_httpd')).
:- use_module(library('http/html_write')).
:- use_module(library('http/mimetype')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/http_session')).
:- use_module(library('http/http_host')).
:- use_module(http_data).
:- use_module(library(settings)).
:- use_module(user_db).
:- use_module(library(debug)).
:- use_module(http_admin).
:- use_module(library('semweb/rdf_db')).
:- use_module(library(url)).

:- http_handler(root('.'),
		http_redirect(moved, location_by_id(serql_home)),
		[]).
:- http_handler(serql('home.html'),		serql_home,		 []).
:- http_handler(serql('sidebar.html'),		sidebar,		 []).
:- http_handler(serql('welcome.html'),		welcome,		 []).
:- http_handler(serql('user/statistics'),	statistics,		 []).
:- http_handler(serql('user/construct'),	construct_form,		 []).
:- http_handler(serql('user/query'),		query_form,		 []).
:- http_handler(serql('user/select'),		select_form,		 []).
:- http_handler(serql('user/loadFile'),		load_file_form,		 []).
:- http_handler(serql('user/loadURL'),		load_url_form,		 []).
:- http_handler(serql('user/loadBaseOntology'),	load_base_ontology_form, []).
:- http_handler(serql('user/clearRepository'),	clear_repository_form,	 []).
:- http_handler(serql('user/removeStatements'),	remove_statements_form,	 []).

:- http_handler(serql('documentation.html'),
		http_reply_file(serql('serql.html'), []), [id(serql_doc)]).
:- http_handler(serql('css/rdfql.css'),
		http_reply_file(serql('rdfql.css'), []), [id(rdfql_css)]).


%%	serql_home(+Request)
%
%	Print the home page.
%
%	NOTE: a frameset must _not_ have a body!

serql_home(_Request) :-
	(   setting(serql_parms:title, Title)
	->  true
	;   Title = 'SWI-Prolog Semantic Web Server'
	),
	reply_html_page([ title(Title),
			  frameset([cols('200,*')],
				   [ frame([ src(location_by_id(sidebar)),
					     name(sidebar)
					   ]),
				     frame([ src(location_by_id(welcome)),
					     name(main)
					   ])
				   ])
			], []).

%%	sidebar(+Request)
%
%	HTTP handler to emit the left bar menu (frame content).

sidebar(_Request) :-
	findall(Path-Label, action(Path, Label), Actions),
	reply_page('Sidebar',
		   [ \current_user,
		     hr([]),
		     \action_by_id(welcome, 'Home'),
		     \cond_action(login),
		     \cond_action(logout),
		     \cond_action(change_password),
		     hr([])
		   | \actions(Actions)
		   ]).

:- multifile
	serql_http:sidebar_menu/2.

%%	action(?Id, ?Label) is nondet.
%
%	True if Id/Label must appear in the side-menu.  Id is one of
%
%	    * handler-id
%	    * -
%	    * HTML code

action(query_form,		'Query database').
action(-,-).
action(load_file_form,	 	'Upload a file').
action(load_url_form,		'Load from HTTP').
action(load_base_ontology_form, 'Load base ontology').
action(-,-).
action(remove_statements_form,  'Remove statements').
action(clear_repository_form,	'Clear the repository').
action(-,-).
action(statistics,		'Statistics').
action(list_users,		'Users ...').
action(settings,		'Settings ...').
action(serql_doc,		'Documentation').
action(Path, Label) :-
	serql_http:sidebar_menu(Path, Label).


current_user -->
	{ catch(logged_on(User), _, fail),
	  (   user_property(User, realname(RealName))
	  ->  true
	  ;   RealName = User
	  ),
	  user_property(User, url(URL))
	}, !,
	html(center(i(a([target(main), href(URL)], RealName)))).
current_user -->
	html(center(font(color(red), i('<not logged in>')))).

cond_action(login) -->
	(   { someone_logged_on }
	->  []
	;   action_by_id(login_form, 'Login')
	).
cond_action(logout) -->
	(   { someone_logged_on }
	->  action_by_id(user_logout, 'Logout')
	;   []
	).
cond_action(change_password) -->
	(   { someone_logged_on }
	->  action_by_id(change_password_form, 'Change password')
	;   []
	).

someone_logged_on :-
	logged_on(User, X),
	X \== User.

%%	welcome(+Request)
%
%	Reply with the normal welcome page.  If there is no user we
%	reply with the `create admin user' page.

welcome(Request) :-
	(   current_user(_)
	->  http_reply_file(serql('welcome.html'),
			    [cache(false)],
			    Request)
	;   http_redirect(moved_temporary,
			  location_by_id(create_admin),
			  Request)
	).


		 /*******************************
		 *	    STATISTICS		*
		 *******************************/

%%	statistics(+Request)
%
%	Provide elementary statistics on the server.

statistics(_Request) :-
	findall(File-Triples,
		rdf_statistics(triples_by_file(File, Triples)),
		UnsortedPairs),
	findall(Index-Count,
		rdf_statistics(lookup(Index, Count)),
		Lookup),
	rdf_statistics(triples(Total)),
	rdf_statistics(core(Core)),
	sort(UnsortedPairs, Pairs),
	gethostname(Host),
	reply_page('RDF statistics',
		   [ h1([id(stattitle)], ['RDF statistics for ', Host]),
		     ol([id(toc)],
			[
			 li(a([href('#ntriples')],'Triples in database')),
			 li(a([href('#callstats')],'Call statistics')),
			 li(a([href('#sessions')],'Active sessions')),
			 li(a([href('#serverstats')],'Server statistics'))
			]),
		     h4([id(ntriples)], 'Triples in database'),
		     p('The RDF store contains ~D triples in ~D bytes memory'-[Total, Core]),
		     table([ id(filesourcetable),
			     border(1),
			     cellpadding(2)
			   ],
			   [ tr([ th('Source'), th(colspan(2), 'Triples') ])
			   | \triples_by_file(Pairs, Total)
			   ]),
		     h4([id(callstats)],'Call statistics'),
		     table([ border(1),
			     cellpadding(2)
			   ],
			   [ tr([ th(colspan(3), 'Indexed'),
				  th('Calls')
				]),
			     \lookup_statistics(Lookup)
			   ]),
		     \current_sessions,
		     \server_statistics
		   ]).

triples_by_file([], Total) -->
	html(tr([ th([align(right), id(total)], 'Total:'),
		  \nc('~D', Total)
		])).
triples_by_file([File-Triples|T], Total) -->
	html(tr([ td(align(right), a(href(File), File)),
		  \nc('~D', Triples),
		  td(\unload_button(File))
		])),
	triples_by_file(T, Total).


unload_button(File) -->
	html(a(href(location_by_id(unload_source) +
		    '?resultFormat=html&source=' +
		    encode(File)),
	       'Unload')).

lookup_statistics([]) -->
	[].
lookup_statistics([rdf(S,P,O)-Count|T]) -->
	html(tr([ td(S), td(P), td(O), \nc('~D', Count)])),
	lookup_statistics(T).

%	current_sessions//0
%
%	Create a table of currently logged on users.

current_sessions -->
	{ findall(S, session(S), Sessions0),
	  sort(Sessions0, Sessions),
	  Sessions \== [], !
	},
	html([ h4([id(sessions)], 'Active sessions'),
	       table([ id(sessiontable),
		       border(1),
		       cellpadding(2)
		     ],
		     [ %caption('Active sessions'),
		       tr([th('User'), th('Real Name'), th('On since'), th('Idle'), th('From')])
		     | \sessions(Sessions)
		     ])
	     ]).
current_sessions -->
	html(p('No users logged in')).

session(s(Idle, User, SessionID, Peer)) :-
	http_current_session(SessionID, peer(Peer)),
	http_current_session(SessionID, idle(Idle)),
	(   user_property(User, session(SessionID))
	->  true
	;   User = (-)
	).

sessions([]) --> [].
sessions([H|T]) --> session(H), sessions(T).

session(s(Idle, -, _SessionID, Peer)) -->
	html(tr([td(-), td(-), td(-), td(\idle(Idle)), td(\ip(Peer))])).
session(s(Idle, User, _SessionID, Peer)) -->
	{  (   user_property(User, realname(RealName))
	   ->  true
	   ;   RealName = '?'
	   ),
	   (   user_property(User, connection(OnSince, _Idle))
	   ->  true
	   ;   OnSince = 0
	   )
	},
	html(tr([td(User), td(RealName), td(\date(OnSince)), td(\idle(Idle)), td(\ip(Peer))])).

idle(Time) -->
	{ Secs is round(Time),
	  Min is Secs // 60,
	  Sec is Secs mod 60
	},
	html('~`0t~d~2|:~`0t~d~5|'-[Min, Sec]).

date(Date) -->
	{ format_time(string(S), '%+', Date)
	},
	html(S).

ip(ip(A,B,C,D)) --> !,
	html('~d.~d.~d.~d'-[A,B,C,D]).
ip(IP) -->
	html('~w'-[IP]).


%	server_statistics//0
%
%	Provide statistics on the HTTP server

server_statistics -->
	{ serql_server_property(port(Port)),
	  serql_server_property(started(StartTime)),
	  format_time(string(ST), '%+', StartTime),
	  http_workers(Port, NWorkers),
	  findall(ID, http_current_worker(Port, ID), Workers),
	  statistics(heapused, Heap)
	},
	html([ h4([id(serverstats)], 'Server statistics'),
	       table([ border(1),
		       cellpadding(2)
		     ],
		     [ tr([ th([align(right), colspan(3)], 'Port:'),
			    td(colspan(3), Port)
			  ]),
		       tr([ th([align(right), colspan(3)], 'Started:'),
			    td(colspan(3), ST)
			  ]),
		       tr([ th([align(right), colspan(3)], 'Heap memory:'),
			    \nc('~D', Heap, [align(left), colspan(3)])
			  ]),
		       tr([ th([align(right), colspan(3)], '# worker threads:'),
			    td(colspan(3), NWorkers)
			  ]),
		       tr(th(colspan(6), 'Statistics by worker')),
		       tr([ th('Thread'),
			    th('CPU'),
			    th(''),
			    th('Local'),
			    th('Global'),
			    th('Trail')
			  ])
		     | \http_workers(Workers)
		     ])
	     ]).

http_workers([]) -->
	[].
http_workers([H|T]) -->
	{ thread_statistics(H, locallimit, LL),
	  thread_statistics(H, globallimit, GL),
	  thread_statistics(H, traillimit, TL),
	  thread_statistics(H, localused, LU),
	  thread_statistics(H, globalused, GU),
	  thread_statistics(H, trailused, TU),
	  thread_statistics(H, cputime, CPU),
	  sformat(Time, '~2f', [CPU])
	},
	html([ tr([ td(rowspan(2), H),
		    td([rowspan(2), align(right)], Time),
		    th('In use'),
		    \nc('~D', LU),
		    \nc('~D', GU),
		    \nc('~D', TU)
		  ]),
	       tr([ th('Limit'),
		    \nc('~D', LL),
		    \nc('~D', GL),
		    \nc('~D', TL)
		  ])
	     ]),
	http_workers(T).

%%	construct_form(+Request)
%
%	Provide a page for issuing a =CONSTRUCT= query.

construct_form(_Request) :-
	catch(logged_on(User), _, User=anonymous),
	reply_page('Specify a query',
		   [ h1(align(center), 'Interactive SeRQL CONSTRUCT query'),

		     p(['A ', \serql_doc_link('CONSTRUCT'),
			' generates an RDF graph']),

		     form([ name(query),
			    action(location_by_id(evaluate_graph_query)),
			    method('GET')
			  ],
			  [ \hidden(repository, default),
			    table(align(center),
				  [ \store_recall(User, construct, 3-2),
				    tr([ td(colspan(6),
					    textarea([ name(query),
						       rows(15),
						       cols(80)
						     ],
						     'CONSTRUCT '))
				       ]),
				    tr([ td([ \small('QLang: '),
					      \query_language
					    ]),
					 td([ \small('Format: '),
					      \result_format
					    ]),
					 td([ \small('Serial.: '),
					      \serialization
					    ]),
					 td([ \small('Res.: '),
					      \resource_menu
					    ]),
					 td([ \small('Entail.: '),
					      \entailment
					    ]),
					 td(align(right),
					    [ input([ type(reset),
						      value('Reset')
						    ]),
					      input([ type(submit),
						      value('Go!')
						    ])
					    ])
				       ])
				  ])
			  ]),
		     \script
		   ]).

store_recall(anonymous, _, _) -->
	[].
store_recall(User, Type, SL-SR) -->
	html(tr([ td(colspan(SL),
		     [ b('Store as: '),
		       input([ name(storeAs),
			       size(40)
			     ])
		     ]),
		  td([ colspan(SR),
		       align(right)
		     ],
		     \recall(User, Type))
		])).


recall(User, Type) -->
	{ findall(Name-Query, stored_query(Name, User, Type, Query), Pairs),
	  Pairs \== []
	},
	html([ b('Recall: '),
	       select(name(recall),
		      [ option([selected], '')
		      | \stored_queries(Pairs, 1)
		      ])
	     ]).
recall(_, _) -->
	[].

stored_queries([], _) -->
	[].
stored_queries([Name-Query|T], I) -->
	{ I2 is I + 1,
	  atom_concat(f, I, FName),
	  js_quoted(Query, QuotedQuery),
	  sformat(Script,
		  'function ~w()\n\
		   { document.query.query.value=\'~w\';\n\
		   }\n',
		  [ FName, QuotedQuery ]),
	  assert(script_fragment(Script)),
	  sformat(Call, '~w()', [FName])
	},
	html(option([onClick(Call)], Name)),
	stored_queries(T, I2).


:- thread_local
	script_fragment/1.

script -->
	{ findall(S, retract(script_fragment(S)), Fragments),
	  Fragments \== []
	}, !,
	[ '\n<script language="JavaScript">\n'
	],
	Fragments,
	[ '\n</script>\n'
	].
script -->
	[].

%%	js_quoted(+Raw, -Quoted)
%
%	Quote text for use in JavaScript. Quoted does _not_ include the
%	leading and trailing quotes.

js_quoted(Raw, Quoted) :-
	atom_codes(Raw, Codes),
	phrase(js_quote_codes(Codes), QuotedCodes),
	atom_codes(Quoted, QuotedCodes).

js_quote_codes([]) -->
	[].
js_quote_codes([0'\r,0'\n|T]) --> !,
	"\\n",
	js_quote_codes(T).
js_quote_codes([H|T]) -->
	js_quote_code(H),
	js_quote_codes(T).

js_quote_code(0'') --> !,
	"\\'".
js_quote_code(0'\\) --> !,
	"\\\\".
js_quote_code(0'\n) --> !,
	"\\n".
js_quote_code(0'\r) --> !,
	"\\r".
js_quote_code(0'\t) --> !,
	"\\t".
js_quote_code(C) -->
	[C].

%%	query_form(+Request)
%
%	Provide a page for issuing a =SELECT= query.

query_form(_Request) :-
	catch(logged_on(User), _, User=anonymous),
	reply_page('Specify a query',
		   [ form([ name(query),
			    action(location_by_id(evaluate_query)),
			    method('GET')
			  ],
			  [ \hidden(repository, default),
			    \hidden(serialization, rdfxml),
			    h1(align(center),
			       [ 'Interactive ',
				 \query_language,
				 ' query'
			       ]),
			    table(align(center),
				  [ \store_recall(User, select, 3-2),
				    tr([ td(colspan(5),
					    textarea([ name(query),
						       rows(15),
						       cols(80)
						     ],
						     ''))
				       ]),
				    tr([ td([ \small('Result format: '),
					      \result_format
					    ]),
					 td([ \small('Resource: '),
					      \resource_menu
					    ]),
					 td([ \small('Entailment: '),
					      \entailment
					    ]),
					 td(align(right),
					    [ input([ type(reset),
						      value('Reset')
						    ]),
					      input([ type(submit),
						      value('Go!')
						    ])
					    ])
				       ])
				  ])
			  ]),
		     \script
		   ]).


%%	select_form(+Request)
%
%	Provide a page for issuing a =SELECT= query

select_form(_Request) :-
	catch(logged_on(User), _, User=anonymous),
	reply_page('Specify a query',
		   [ h1(align(center), 'Interactive SeRQL SELECT query'),

		     p(['A ', \serql_doc_link('SELECT'),
			' generates a table']),

		     form([ name(query),
			    action(location_by_id(evaluate_table_query)),
			    method('GET')
			  ],
			  [ \hidden(repository, default),
			    \hidden(serialization, rdfxml),
			    table(align(center),
				  [ \store_recall(User, select, 3-2),
				    tr([ td(colspan(6),
					    textarea([ name(query),
						       rows(15),
						       cols(80)
						     ],
						     'SELECT '))
				       ]),
				    tr([ td([ \small('Result format: '),
					      \result_format
					    ]),
					 td([ \small('Language: '),
					      \query_language
					    ]),
					 td([ \small('Resource: '),
					      \resource_menu
					    ]),
					 td([ \small('Entailment: '),
					      \entailment
					    ]),
					 td(align(right),
					    [ input([ type(reset),
						      value('Reset')
						    ]),
					      input([ type(submit),
						      value('Go!')
						    ])
					    ])
				       ])
				  ])
			  ]),
		     \script
		   ]).


serql_doc_link(Label) -->
	{ setting(serql_parms:serql_documentation_url, URL)
	},
	html(a([href(URL)], Label)).

serialization -->
	html(select(name(serialization),
		    [ option([selected], rdfxml),
		      option([], ntriples),
		      option([], n3)
		    ])).

result_format -->
	html(select(name(resultFormat),
		    [ option([], xml),
		      option([selected], html)/*,
		      option([], rdf)*/
		    ])).

query_language -->
	html(select(name(queryLanguage),
		    [ option([selected], 'SPARQL'),
		      option([],         'SeRQL')
		    ])).

resource_menu -->
	html(select(name(resourceFormat),
		    [ option([value(plain)], 		plain),
		      option([value(ns), selected],	'ns:local'),
		      option([value(nslabel)], 	'ns:label')
		    ])).

entailment -->
	{ findall(E, serql:entailment(E, _), Es)
	},
	html(select(name(entailment),
		    \entailments(Es))).

entailments([]) -->
	[].
entailments([E|T]) -->
	(   { setting(serql_parms:default_entailment, E)
	    }
	->  html(option([selected], E))
	;   html(option([], E))
	),
	entailments(T).

small(Text) -->
	html(font(size(-1), Text)).


%%	load_file_form(+Request)
%
%	Provide a form for uploading triples from a local file.

load_file_form(_Request) :-
	reply_page('Upload RDF',
		   [ h3(align(center), 'Upload an RDF document'),

		     p(['Upload a document using POST to /servlets/uploadData. \
		         Alternatively you can use ',
			 a(href=loadURL,loadURL), ' to load data from a \
			 web server.'
		       ]),

		     form([ action(location_by_id(upload_data)),
			    method('POST'),
			    enctype('multipart/form-data')
			  ],
			  [ \hidden(resultFormat, html),
			    table([tr([ td(align(right), 'File:'),
					td(input([ name(data),
						   type(file),
						   size(50)
						 ]))
				      ]),
				   tr([ td(align(right), 'BaseURI:'),
					td(input([ name(baseURI),
						   size(50)
						 ]))
				      ]),
				   tr([ td([align(right), colspan(2)],
					   input([ type(submit),
						   value('Upload now')
						 ]))
				      ])
				  ])
			  ])
		   ]).


%%	load_url_form(+Request)
%
%	Provide a form for uploading triples from a URL.

load_url_form(_Request) :-
	reply_page('Load RDF from HTTP server',
		   [ h3(align(center), 'Load RDF from HTTP server'),
		     form([ action(location_by_id(upload_url)),
			    method('GET')
			  ],
			  [ \hidden(resultFormat, html),
			    table([tr([ td(align(right), 'URL:'),
					td(input([ name(url),
						   value('http://'),
						   size(50)
						 ]))
				      ]),
				   tr([ td(align(right), 'BaseURI:'),
					td(input([ name(baseURI),
						   size(50)
						 ]))
				      ]),
				   tr([ td([align(right), colspan(2)],
					   input([ type(submit),
						   value('Upload now')
						 ]))
				      ])
				  ])
			  ])
		   ]).


%%	load_base_ontology_form(+Request)
%
%	Provide a form for loading an ontology from the archive.

load_base_ontology_form(Request) :- !,
	authorized(read(status, listBaseOntologies)),
	reply_page('Load base ontology',
		   [ h3(align(center), 'Load ontology from repository'),

		     p('This page allows loading one of the ontologies \
		        provided with the toolkit.'),

		     form([ action(location_by_id(load_base_ontology)),
			    method('GET')
			  ],
			  [ \hidden(resultFormat, html),
			    b('Ontology'),
			    select(name(ontology),
				   [ option([], '')
				   | \base_ontologies(Request)
				   ]),
			    input([ type(submit),
				    value('Load')
				  ])
			  ])
		   ]).


base_ontologies(Request) -->
	{ get_base_ontologies(Request, Rows) },
	emit_base_ontologies(Rows).

get_base_ontologies(_Request, List) :-
	catch(findall(row(O), serql_base_ontology(O), List), _, fail), !.
get_base_ontologies(Request, Rows) :-
	http_current_host(Request, Host, Port, []),
	http_location_by_id(list_base_ontologies, ListBaseOntos),
	debug(base_ontologies, 'Opening http://~w:~w~w',
	      [Host, Port, ListBaseOntos]),
	http_open([ protocol(http),
		    host(Host),
		    port(Port),
		    path(ListBaseOntos),
		    search([resultFormat(xml)])
		  ],
		  In,
		  [ % request_header('Cookie', Cookie)
		  ]),
	debug(base_ontologies, '--> Reading from ~w', [In]),
	xml_read_result_table(In, Rows, _VarNames).

emit_base_ontologies([]) -->
	[].
emit_base_ontologies([row(H)|T]) -->
	html(option([], H)),
	emit_base_ontologies(T).


%%	clear_repository_form(+Request)
%
%	HTTP handle presenting a form to clear the repository.

clear_repository_form(_Request) :-
	reply_page('Load base ontology',
		   [ h3(align(center), 'Clear entire repository'),

		     p(['This operation removes ', b(all), ' triples from \
		         the RDF store.']),

		     form([ action(location_by_id(clear_repository)),
			    method('GET')
			  ],
			  [ \hidden(repository, default),
			    \hidden(resultFormat, html),
			    input([ type(submit),
				    value('Clear repository now')
				  ])
			  ])
		   ]).


%%	remove_statements_form(+Request)
%
%	HTTP handler providing a form to remove RDF statements.

remove_statements_form(_Request) :-
	reply_page('Load base ontology',
		   [ h3(align(center), 'Remove statements'),

		     p('Remove matching triples from the database.  The three \
		        fields are in ntriples notation.  Omitted fields \
			match any value.'),

		     form([ action(location_by_id(remove_statements)),
			    method('GET')
			  ],
			  [ \hidden(repository, default),
			    \hidden(resultFormat, html),
			    table([ tr([ th(align(right), 'Subject: '),
					 td(input([ name(subject),
						    size(50)
						  ]))
				       ]),
				    tr([ th(align(right), 'Predicate: '),
					 td(input([ name(predicate),
						    size(50)
						  ]))
				       ]),
				    tr([ th(align(right), 'Object: '),
					 td(input([ name(object),
						    size(50)
						  ]))
				       ]),
				    tr([ td([ align(right),
					      colspan(2)
					    ],
					    input([ type(submit),
						    value('Remove')
						  ]))
				       ])
				  ])
			  ])
		   ]).


		 /*******************************
		 *		UTIL		*
		 *******************************/

actions([]) -->
	[].
actions([Path-Label|T]) -->
	action(Path, Label),
	actions(T).

%%	action(+Action, +Label)// is det
%
%	Add an action to the sidebar.  Action is one of
%
%		$ =-= :
%		Add a horizontal rule (<hr>)
%		$ Atom :
%		ID of an HTTP handler. For backward compatibility we
%		also accept an HTTP url with a warning.  The location
%		is opened in the window named =main=.
%		$ HTML DOM :
%		Insert given HTML

action(-, -) --> !,
	html(hr([])).
action(-, Label) --> !,
	html([ hr([]),
	       center(b(Label)),
	       hr([])
	     ]).
action(Spec, Label) -->
	{ atom(Spec) }, !,
	{ (   \+ sub_atom(Spec, 0, _, _, 'http://'),
	      catch(http_location_by_id(Spec, Location), E,
		    (   print_message(informational, E),
			fail))
	  ->  true
	  ;   Location = Spec
	  )
	},
	html([a([target(main), href(Location)], Label), br([])]).
action(Action, _) -->
	html(Action),
	html(br([])).

action_by_id(ID, Label) -->
	{ http_location_by_id(ID, Location) },
	html([a([target(main), href(Location)], Label), br([])]).

%%	nc(+Format, +Value)// is det.
%
%	Numeric  cell.  The  value  is    formatted   using  Format  and
%	right-aligned in a table cell (td).

nc(Fmt, Value) -->
	nc(Fmt, Value, []).

nc(Fmt, Value, Options) -->
	{ format(string(Txt), Fmt, [Value]),
	  (   memberchk(align(_), Options)
	  ->  Opts = Options
	  ;   Opts = [align(right)|Options]
	  )
	},
	html(td(Opts, Txt)).


%%	hidden(+Name, +Value)// is det.
%
%	Create a hidden input field with given name and value

hidden(Name, Value) -->
	html(input([ type(hidden),
		     name(Name),
		     value(Value)
		   ])).


server_url(Local, URL) :-
	setting(http:server_url, Base),
	atom_concat(Base, Local, URL).


		 /*******************************
		 *		EMIT		*
		 *******************************/

reply_page(Title, Content) :-
	phrase(page(title(Title), Content), HTML),
	format('Content-type: text/html~n~n'),
	print_html(HTML).


                 /*******************************
                 *        PCEEMACS SUPPORT      *
                 *******************************/

:- multifile
        emacs_prolog_colours:goal_colours/2,
        prolog:called_by/2.


emacs_prolog_colours:goal_colours(reply_page(_, HTML),
                                  built_in-[classify, Colours]) :-
        catch(html_write:html_colours(HTML, Colours), _, fail).

prolog:called_by(reply_page(_, HTML), Called) :-
        catch(phrase(html_write:called_by(HTML), Called), _, fail).
