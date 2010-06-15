/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2010, University of Amsterdam
			      VU University Amsterdam

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

:- module(http_user,
	  [ serql_page/2,		% +Title, +Content
	    cp_menu//0			% ClioPatria menu
	  ]).

:- use_module(server).
:- use_module(xml_result).
:- use_module(library(http/http_open)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/mimetype)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_host)).
:- use_module(http_data).
:- use_module(library(settings)).
:- use_module(user_db).
:- use_module(library(debug)).
:- use_module(http_admin).
:- use_module(http_stats).
:- use_module(http_browse).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(url)).
:- use_module(library(occurs)).
:- use_module(library(pairs)).

:- http_handler(root('.'),
		http_redirect(moved, location_by_id(serql_home)),
		[priority(-100)]).
:- http_handler(serql('home.html'),		welcome,
		[id(serql_home)]).
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
		reply_decorated_file(serql('serql.html')), [id(serql_doc)]).


cp_menu -->
	{ findall(Key-Item, current_menu_item(Key, Item), Pairs0),
	  sort(Pairs0, Pairs),
	  group_pairs_by_key(Pairs, ByKey),
	  sort_menu_popups(ByKey, Menu)
	},
	html_requires(css('menu.css')),
	html(ul(id(nav),
		\menu(Menu))).

menu([]) --> !.
menu([_-[Item]|T]) --> !,
	menu_item(Item),
	menu(T).
menu([Key-Items|T]) -->
	{ menu_label(Key, Key, Label) },
	html(li([ a([Label]),
		  ul(\menu_items(Items))
		])),
	menu(T).

menu_items([]) --> [].
menu_items([H|T]) --> menu_item(H), menu_items(T).

menu_item(item(Spec, Label)) -->
	{ atom(Spec) }, !,
	{ (   \+ sub_atom(Spec, 0, _, _, 'http://'),
	      catch(http_location_by_id(Spec, Location), E,
		    (   print_message(informational, E),
			fail))
	  ->  true
	  ;   Location = Spec
	  )
	},
	html(li(a([href(Location)], Label))).


current_menu_item(Key, item(Location, Label)) :-
	menu_item(Where, DefLabel),
	(   Where = Key/Location
	->  menu_label(Location, DefLabel, Label)
	;   Where = Location,
	    Key = Location,
	    menu_label(Location, DefLabel, Label)
	).


:- multifile
	serql_http:sidebar_menu/2,	% +Action, +Label
	serql_http:menu_label/2,	% +Id, -Label
	menu_item/2,			% +Action, +Label
	menu_popup_order/2.

%%	menu_item(?Id, ?Label) is nondet.
%
%	True if Id/Label must appear in the side-menu.  Id is one of
%
%	    * handler-id
%	    * HTML code

menu_item(file/load_file_form,	 	'Load local file').
menu_item(file/load_url_form,		'Load from HTTP').
menu_item(file/load_base_ontology_form, 'Load base ontology').
menu_item(file/remove_statements_form,  'Remove statements').
menu_item(file/clear_repository_form,	'Clear the repository').
menu_item(query/query_form,		'Query').
menu_item(view/list_graphs,		'Graphs').
menu_item(view/statistics,		'Statistics').
menu_item(admin/list_users,		'Users').
menu_item(admin/settings,		'Settings').
% Old-style application menus
menu_item(application/Path, Label) :-
	serql_http:sidebar_menu(Path, Label).
% Home location
menu_item(help/serql_home,		'Home').
menu_item(help/serql_doc,		'Documentation').
% Keep users at the end
menu_item(user/login_form,		'Login') :-
	\+ someone_logged_on.
menu_item(current_user/user_logout,	'Logout') :-
	someone_logged_on.
menu_item(current_user/change_password_form,	'Change password') :-
	someone_logged_on.

sort_menu_popups(List, Sorted) :-
	map_list_to_pairs(popup_order, List, Keyed),
	keysort(Keyed, KeySorted),
	pairs_values(KeySorted, Sorted).

popup_order(Key-Members, Order-(Key-Members)) :-
	(   menu_popup_order(Key, Order)
	->  true
	;   Order = 10000
	).

menu_popup_order(file,	       1000).
menu_popup_order(query,	       2000).
menu_popup_order(view,	       3000).
menu_popup_order(admin,	       4000).
menu_popup_order(application,  5000).
menu_popup_order(help,	       6000).
menu_popup_order(user,	       7000).
menu_popup_order(current_user, 8000).

menu_label(Item, _Default, Label) :-
	serql_http:menu_label(Item, Label), !.
menu_label(serql_home, _Default, 'Home') :- !.
menu_label(current_user, _Default, Label) :-
	logged_on(User, X),
	X \== User, !,
	(   user_property(User, realname(RealName))
	->  true
	;   RealName = User
	),
	(   user_property(User, url(URL))
	->  Label = a(href(URL), i(RealName))
	;   Label = i(RealName)
	).
menu_label(_, Default, Label) :-
	capitalize(Default, Label).

capitalize(Atom, Capital) :-
	atom_codes(Atom, Codes),
	(   maplist(is_upper, Codes)
	->  Capital = Atom
	;   Codes = [First|Rest]
	->  code_type(First, to_lower(Up)),
	    UpCodes = [Up|Rest],
	    atom_codes(Capital, UpCodes)
	;   Capital = Atom
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
	->  reply_decorated_file(serql('welcome.html'), Request)
	;   http_redirect(moved_temporary,
			  location_by_id(create_admin),
			  Request)
	).


%%	reply_decorated_file(+Alias, +Request) is det.
%
%	Present an HTML file embedded using  the server styling. This is
%	achieved by parsing the  HTML  and   passing  the  parsed DOM to
%	serql_page/2.

reply_decorated_file(Alias, _Request) :-
	absolute_file_name(Alias, Page, [access(read)]),
	load_html_file(Page, DOM),
	contains_term(element(title, _, Title), DOM),
	contains_term(element(body, _, Body), DOM),
	Style = element(style, _, _),
	findall(Style, sub_term(Style, DOM), Styles),
	append(Styles, Body, Content),
	serql_page(title(Title), Content).


		 /*******************************
		 *	    STATISTICS		*
		 *******************************/

%%	statistics(+Request)
%
%	Provide elementary statistics on the server.

statistics(Request) :-
	http_current_host(Request, Host, _Port, [global(true)]),
	serql_page(title('RDF statistics'),
		   div(id('rdf-statistics'),
		       [ h1([id(stattitle)], ['RDF statistics for ', Host]),
			 ol([id(toc)],
			    [ li(a(href('#ntriples'),    'Triples in database')),
			      li(a(href('#callstats'),   'Call statistics')),
			      li(a(href('#sessions'),    'Active sessions')),
			      li(a(href('#serverstats'), 'Server statistics'))
			    ]),
			 h4([id(ntriples)], 'Triples in database'),
			 \triple_statistics,
			 h4([id(callstats)],'Call statistics'),
			 \rdf_call_stat_table,
			 h4([id(sessions)], 'Active sessions'),
			 \http_session_table,
			 h4([id(serverstats)], 'Server statistics'),
			 p('Static workers and statistics:'),
			 \http_server_statistics,
			 p('Defined dynamic worker pools:'),
			 \http_server_pool_table
		       ])).


triple_statistics -->
	{ rdf_statistics(triples(Total)),
	  rdf_statistics(core(Core)),
	  graph_count(Count),
	  http_link_to_id(list_graphs, [], ListGraphs)
	},
	html(p([ 'The RDF store contains ~D triples '-[Total],
		 'in ~D '-[Count], a(href(ListGraphs), graphs),
		 ', using ~D bytes memory'-[Core]])).


graph_count(Count) :-
	aggregate_all(count, rdf_graph(_), Count).


%%	construct_form(+Request)
%
%	Provide a page for issuing a =CONSTRUCT= query.

construct_form(_Request) :-
	catch(logged_on(User), _, User=anonymous),
	serql_page(title('Specify a query'),
		   [ h1(align(center), 'Interactive SeRQL CONSTRUCT query'),

		     p(['A CONSTRUCT generates an RDF graph']),

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
	  format(atom(Script),
		 'function ~w()\n\
		 { document.query.query.value=\'~w\';\n\
		 }\n',
		 [ FName, QuotedQuery ]),
	  assert(script_fragment(Script)),
	  format(atom(Call), '~w()', [FName])
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
	serql_page(title('Specify a query'),
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
				  [ \store_recall(User, _, 3-2),
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
				  ]),
			    \query_docs
			  ]),
		     \script
		   ]).


query_docs -->
	html(ul([ li(a(href('http://www.w3.org/TR/rdf-sparql-query/'),
		       'SPARQL Documentation')),
		  li(a(href('http://www.openrdf.org/'),
		       'Sesame and SeRQL site'))
		])).


%%	select_form(+Request)
%
%	Provide a page for issuing a =SELECT= query

select_form(_Request) :-
	catch(logged_on(User), _, User=anonymous),
	serql_page(title('Specify a query'),
		   [ h1(align(center), 'Interactive SeRQL SELECT query'),

		     p(['A SELECT generates a table']),

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
	serql_page(title('Upload RDF'),
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
	serql_page(title('Load RDF from HTTP server'),
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
	get_base_ontologies(Request, Ontologies),
	serql_page(title('Load base ontology'),
		   [ h3(align(center), 'Load ontology from repository'),

		     p('Select an ontology from the registered libraries'),

		     \load_base_ontology_form(Ontologies)
		   ]).

%%	load_base_ontology_form(+Ontologies)//
%
%	HTML component that emits a form to load a base-ontology and its
%	dependencies. Ontologies is a list   of  ontology-identifiers as
%	used by rdf_load_library/1.

load_base_ontology_form(Ontologies) -->
	html_requires(css('rdfql.css')),
	html(form([ action(location_by_id(load_base_ontology)),
		    method('GET')
		  ],
		  [ \hidden(resultFormat, html),
		    table([ id('load-base-ontology-form'),
			    class(rdfql)
			  ],
			  [ tr([ th('Ontology'),
				 td(select(name(ontology),
					   [ option([], '')
					   | \emit_base_ontologies(Ontologies)
					   ]))
			       ]),
			    tr(class(buttons),
			       td([colspan(2), align(right)],
				  input([ type(submit),
					  value('Load')
					])))
			  ])
		  ])).


emit_base_ontologies([]) -->
	[].
emit_base_ontologies([H|T]) -->
	(   { rdf_library_index(H, title(Title)) }
	->  html(option([value(H)], [H, ' -- ', Title]))
	;   html(option([value(H)], H))
	),
	emit_base_ontologies(T).


get_base_ontologies(_Request, List) :-
	catch(findall(O, serql_base_ontology(O), List), _, fail), !.
get_base_ontologies(Request, List) :-
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
	xml_read_result_table(In, Rows, _VarNames),
	maplist(arg(1), Rows, List).

%%	clear_repository_form(+Request)
%
%	HTTP handle presenting a form to clear the repository.

clear_repository_form(_Request) :-
	serql_page(title('Load base ontology'),
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
	serql_page(title('Load base ontology'),
		   [ h3(align(center), 'Remove statements'),

		     p('Remove matching triples from the database.  The three \
		        fields are in ntriples notation.  Omitted fields \
			match any value.'),

		     \remove_statements_form
		   ]).

remove_statements_form -->
	html_requires(css('rdfql.css')),
	html(form([ action(location_by_id(remove_statements)),
		    method('GET')
		  ],
		  [ \hidden(repository, default),
		    \hidden(resultFormat, html),
		    table([ id('remove-statements-form'),
			    class(rdfql)
			  ],
			  [ tr([ th(align(right), 'Subject: '),
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
			    tr(class(buttons),
			       [ td([ align(right),
				      colspan(2)
				    ],
				    input([ type(submit),
					    value('Remove')
					  ]))
			       ])
			  ])
		  ])).


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
	html([a([href(Location)], Label), br([])]).
action(Action, _) -->
	html(Action),
	html(br([])).

action_by_id(ID, Label) -->
	{ http_location_by_id(ID, Location) },
	html([a([href(Location)], Label), br([])]).

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


		 /*******************************
		 *		EMIT		*
		 *******************************/

:- meta_predicate
	serql_page(:, :).

serql_page(Head, Content) :-
	reply_html_page(Head,
			body(class('yui-skin-sam'),
			     [ div(id(sidebar), \cp_menu),
			       \rdf_search_form,
			       br(clear(all)),
			       div(id(content), Content)
			     ])).


                 /*******************************
                 *        PCEEMACS SUPPORT      *
                 *******************************/

:- multifile
        emacs_prolog_colours:goal_colours/2,
        prolog:called_by/2.


emacs_prolog_colours:goal_colours(serql_page(_, HTML),
                                  built_in-[classify, Colours]) :-
        catch(html_write:html_colours(HTML, Colours), _, fail).

prolog:called_by(serql_page(_, HTML), Called) :-
        catch(phrase(html_write:called_by(HTML), Called), _, fail).
