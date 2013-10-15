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

:- module(cpa_user, []).

:- use_module(rdfql(serql_xml_result)).
:- use_module(library(http/http_open)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/mimetype)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_host)).
:- use_module(api(sesame)).
:- use_module(api(rdflib)).
:- use_module(library(settings)).
:- use_module(user(user_db)).
:- use_module(library(debug)).
:- use_module(components(server_statistics)).
:- use_module(components(query)).
:- use_module(components(basics)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(url)).
:- use_module(library(occurs)).
:- use_module(library(pairs)).

/** <Module> Basic user (developer) interaction

This module contains the main front-end of ClioPatria. It notably
provides the HTTP-handlers for / and /home:

    $ / :
    This handler, with id=root, redirects either to /home (id=home) or
    to id=create_admin. The latter is issued if there is no initialised
    user-db.

    $ /home :
    Provides the default welcome page of ClioPatria.

If one develops an application on top   of  ClioPatria, it is adviced to
redefine the handler for =home=, as in:

    ==
    :- http_handler('/welcome', home, []).

    home(Request) :-
	...
    ==

If the application wants to provide  a   link  to the generic ClioPatria
administrative interface, it can do so by   linking  to the id=admin, as
in:

    ==
	...,
	{ http_link_to_id(admin, [], AdminRef) },
	html(a(href(AdminRef), admin)),
	...
    ==
*/

:- http_handler(root('.'),			     root,
		[ priority(-100) ]).
:- http_handler(cliopatria(home),		     home,
		[ priority(-100) ]).
:- http_handler(cliopatria(admin),		     home,
		[ id(admin) ]).
:- http_handler(cliopatria('user/statistics'),	     statistics,	      []).
:- http_handler(cliopatria('user/query'),	     query_form,	      []).
:- http_handler(cliopatria('user/loadFile'),	     load_file_form,	      []).
:- http_handler(cliopatria('user/loadURL'),	     load_url_form,	      []).
:- http_handler(cliopatria('user/loadLibraryRDF'),   load_library_rdf_form, []).
:- http_handler(cliopatria('user/clearRepository'),  clear_repository_form,   []).
:- http_handler(cliopatria('user/removeStatements'), remove_statements_form,  []).


%%	root(+Request)
%
%	Default ClioPatria handler for /.  The default handler redirects
%	to id=home, unless the use-info is not initialised. in that case
%	it redirects to id=create_admin.

root(Request) :-
	(   current_user(_)
	->  http_redirect(moved_temporary,
			  location_by_id(home),
			  Request)
	;   http_redirect(moved_temporary,
			  location_by_id(create_admin),
			  Request)
	).


%%	home(+Request)
%
%	Reply with the normal  welcome  page.   The  welcome  page  is a
%	decorated version of html('welcome.html').

home(Request) :-
	reply_decorated_file(html('welcome.html'), Request).


%%	reply_decorated_file(+Alias, +Request) is det.
%
%	Present an HTML file embedded using  the server styling. This is
%	achieved by parsing the  HTML  and   passing  the  parsed DOM to
%	reply_html_page/3.

reply_decorated_file(Alias, _Request) :-
	absolute_file_name(Alias, Page, [access(read)]),
	load_html_file(Page, DOM),
	contains_term(element(title, _, Title), DOM),
	contains_term(element(body, _, Body), DOM),
	Style = element(style, _, _),
	findall(Style, sub_term(Style, DOM), Styles),
	append(Styles, Body, Content),
	reply_html_page(cliopatria(html_file),
			title(Title), Content).


		 /*******************************
		 *	    STATISTICS		*
		 *******************************/

%%	statistics(+Request)
%
%	Provide elementary statistics on the server.

statistics(Request) :-
	http_current_host(Request, Host, _Port, [global(true)]),
	reply_html_page(cliopatria(default),
			title('RDF statistics'),
			[ div(id('rdf-statistics'),
			      [ h1([id(stattitle)], ['RDF statistics for ', Host]),
				ol([id(toc)],
				   [ li(a(href('#ntriples'),    'Triples in database')),
				     li(a(href('#callstats'),   'Call statistics')),
				     li(a(href('#sessions'),    'Active sessions')),
				     li(a(href('#serverstats'), 'Server statistics'))
				   ]),
				h2([id(ntriples)], 'Triples in database'),
				\triple_statistics,
				h2([id(callstats)],'Call statistics'),
				\rdf_call_statistics_table,
				h2([id(sessions)], 'Active sessions'),
				\http_session_table,
				h2([id(serverstats)], 'Server statistics'),
				h3('Static workers and statistics:'),
				\http_server_statistics,
				h3('Defined dynamic worker pools:'),
				\http_server_pool_table
			      ])
			]).


triple_statistics -->
	{ rdf_statistics(triples(Total)),
	  graph_count(Count),
	  http_link_to_id(list_graphs, [], ListGraphs)
	},
	html(p([ 'The RDF store contains ', \n(human, Total), ' triples in ',
		 \n(human, Count), ' ', a(href(ListGraphs), graphs),
		 \using_core])).

:- if((rdf_version(V),V<30000)).
using_core -->
	{ rdf_statistics(core(Core)) }, !,
	html([', using ', \n(human, Core), 'b memory']).
:- endif.
using_core -->
	[].

graph_count(Count) :-
	aggregate_all(count, rdf_graph(_), Count).

%%	query_form(+Request)
%
%	Provide a page for issuing a =SELECT= query.

query_form(_Request) :-
	reply_html_page(cliopatria(default),
			title('Specify a query'),
			[ \query_form([]),
			  \query_docs,
			  \warn_interactive
			]).



warn_interactive -->
	{ http_location_by_id(sparql_query, HREF),
	  SparqlAPI = 'http://www.w3.org/TR/rdf-sparql-protocol/'
	},
	html([ br(clear(all)),
	       p(class(footnote),
		 [ 'This form is to test SPARQL queries ', i(interactively), '. ',
		   'Machines should use ', b([HREF,'?query=...']),
		   ', which provides a ',
		   a(href(SparqlAPI), 'SPARQL compliant HTTP API'), '.'
		 ])
	     ]).

query_docs -->
	html(ul([ li(a(href('http://www.w3.org/TR/rdf-sparql-query/'),
		       'SPARQL Documentation')),
		  li(a(href('http://www.openrdf.org/'),
		       'Sesame and SeRQL site'))
		])).

%%	load_file_form(+Request)
%
%	Provide a form for uploading triples from a local file.

load_file_form(_Request) :-
	authorized(write(default, load(posted))),
	reply_html_page(cliopatria(default),
			title('Upload RDF'),
			[ h1('Upload an RDF document'),

			  p(['Upload a document using POST to /servlets/uploadData. ',
			     'Alternatively you can use ',
			     a(href=loadURL,loadURL), ' to load data from a \c
			     web server.'
			    ]),

			  form([ action(location_by_id(upload_data)),
				 method('POST'),
				 enctype('multipart/form-data')
			       ],
			       [ \hidden(resultFormat, html),
				 table(class(form),
				       [tr([ th(class(label), 'File:'),
					     td(input([ name(data),
							type(file),
							size(50)
						      ]))
					   ]),
					tr([ th(class(label), 'BaseURI:'),
					     td(input([ name(baseURI),
							size(50)
						      ]))
					   ]),
					tr(class(buttons),
					   [ th([align(right), colspan(2)],
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
	TurtleHREF = 'http://www.w3.org/TeamSubmission/turtle/',
	reply_html_page(cliopatria(default),
			title('Load RDF from HTTP server'),
			[ h1('Load RDF from HTTP server'),
			  p([ 'This form can load RDF from an HTTP server that serves ',
			      'either RDF/XML or ', a(href(TurtleHREF), 'Turtle'), '. ',
			      'The format is derived from the Content-type reported by ',
			      'the server or the file name extension.'
			    ]),
			  form([ action(location_by_id(upload_url)),
				 method('GET')
			       ],
			       [ \hidden(resultFormat, html),
				 table(class(form),
				       [tr([ th(class(label), 'URL:'),
					     td(input([ name(url),
							value('http://'),
							size(50)
						      ]))
					   ]),
					tr([ th(class(label), 'BaseURI:'),
					     td(input([ name(baseURI),
							size(50)
						      ]))
					   ]),
					tr(class(buttons),
					   [ td([align(right), colspan(2)],
						input([ type(submit),
							value('Load RDF')
						      ]))
					   ])
				       ])
			       ])
			]).


%%	load_library_rdf_form(+Request)
%
%	Provide a form  for  loading  an   ontology  from  the  library.
%	Libraries are made  available  through   the  file  search  path
%	=ontology=. Directories found through this   alias  are searched
%	recursively for files named =|Manifest.ttl|=.
%
%	@see file_search_path/2
%	@see rdf_attach_library/1.

load_library_rdf_form(Request) :-
	authorized(read(status, listBaseOntologies)),
	get_base_ontologies(Request, Ontologies),
	reply_html_page(cliopatria(default),
			title('Load base ontology'),
			[ h1('Load ontology from repository'),

			  p('Select an ontology from the registered libraries'),

			  \load_base_ontology_form(Ontologies)
			]).


%%	load_base_ontology_form(+Ontologies)//
%
%	HTML component that emits a form to load a base-ontology and its
%	dependencies. Ontologies is a list   of  ontology-identifiers as
%	used by rdf_load_library/1.

load_base_ontology_form(Ontologies) -->
	html(form([ action(location_by_id(load_library_ontology)),
		    method('GET')
		  ],
		  [ \hidden(resultFormat, html),
		    table(class(form),
			  [ tr([ th('Ontology:'),
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
	catch(findall(O, library_ontology(O), List0), _, fail), !,
	sort(List0, List).
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
	reply_html_page(cliopatria(default),
			title('Load base ontology'),
			[ h1('Clear entire repository'),

			  p(['This operation removes ', b(all), ' triples from \c
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
	reply_html_page(cliopatria(default),
			title('Load base ontology'),
			[ h1('Remove statements'),

			  p(['Remove matching triples from the database.  The three ',
			     'fields are in ntriples/Turtle notation.  Omitted fields ',
			     'match any value.'
			    ]),

			  \remove_statements_form
			]).

remove_statements_form -->
	html(form([ action(location_by_id(remove_statements)),
		    method('GET')
		  ],
		  [ \hidden(repository, default),
		    \hidden(resultFormat, html),
		    table([ class(form)
			  ],
			  [ tr([ th(class(label), 'Subject:'),
				 td(input([ name(subject),
					    size(50)
					  ]))
			       ]),
			    tr([ th(class(label), 'Predicate:'),
				 td(input([ name(predicate),
					    size(50)
					  ]))
			       ]),
			    tr([ th(class(label), 'Object:'),
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
