/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2024, University of Amsterdam,
                              VU University Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(cpa_user, []).

:- use_module(rdfql(serql_xml_result)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_host)).
:- use_module(library(http/cp_jquery)).
:- use_module(api(rdflib)).
:- use_module(user(user_db)).
:- use_module(library(debug)).
:- use_module(components(server_statistics)).
:- use_module(components(query)).
:- use_module(components(basics)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(occurs)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(sgml)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_stream)).

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

:- http_handler(root('.'),                           root,
                [ priority(-100) ]).
:- http_handler(cliopatria(home),                    home,
                [ priority(-100) ]).
:- http_handler(cliopatria(admin),                   home,
                [ id(admin) ]).
:- http_handler(cliopatria('user/query'),            query_form,
                [id(sparql_query_form)]).
:- http_handler(cliopatria('user/statistics'),       statistics,              []).
:- http_handler(cliopatria('user/loadFile'),         load_file_form,          []).
:- http_handler(cliopatria('user/loadURL'),          load_url_form,           []).
:- http_handler(cliopatria('user/loadLibraryRDF'),   load_library_rdf_form,   []).
:- http_handler(cliopatria('user/clearRepository'),  clear_repository_form,   []).
:- http_handler(cliopatria('user/removeStatements'), remove_statements_form,  []).


%!  root(+Request)
%
%   Default ClioPatria handler for /.  The default handler redirects
%   to id=home, unless the use-info is not initialised. in that case
%   it redirects to id=create_admin.

root(Request) :-
    redirect_create_admin(Request),
    http_redirect(moved_temporary,
                  location_by_id(home),
                  Request).

redirect_create_admin(Request) :-
    \+ current_user(_),
    !,
    http_redirect(moved_temporary,
                  location_by_id(create_admin),
                  Request).
redirect_create_admin(_).

%!  home(+Request)
%
%   Reply with the normal  welcome  page.   The  welcome  page  is a
%   decorated version of html('welcome.html').

home(Request) :-
    redirect_create_admin(Request),
    reply_decorated_file(html('welcome.html'), Request).


%!  reply_decorated_file(+Alias, +Request) is det.
%
%   Present an HTML file embedded using  the server styling. This is
%   achieved by parsing the  HTML  and   passing  the  parsed DOM to
%   reply_html_page/3.

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
                 *          STATISTICS          *
                 *******************************/

%!  statistics(+Request)
%
%   Provide elementary statistics on the server.

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
    { rdf_statistics(core(Core)) },
    !,
    html([', using ', \n(human, Core), 'b memory']).
:- endif.
using_core -->
    [].

graph_count(Count) :-
    aggregate_all(count, rdf_graph(_), Count).

:- if(exists_source(library(http/http_server_health))).
:- use_module(library(http/http_server_health)).
:- http_handler(cliopatria(health), server_health, []).
:- else.
:- http_handler(cliopatria('health'), health, []).

%!  health(+Request)
%
%   Provide basic statistics for health checks

%%	health(+Request)
%
%	HTTP handler that replies with the overall health of the server

health(_Request) :-
	get_server_health(Health),
	reply_json(Health).

get_server_health(Health) :-
	findall(Key-Value, health(Key, Value), Pairs),
	dict_pairs(Health, health, Pairs).

health(up, true).
health(uptime, Time) :-
	get_time(Now),
	(   http_server_property(_, start_time(StartTime))
	->  Time is round(Now - StartTime)
	).
health(requests, RequestCount) :-
	cgi_statistics(requests(RequestCount)).
health(bytes_sent, BytesSent) :-
	cgi_statistics(bytes_sent(BytesSent)).
health(open_files, Streams) :-
	aggregate_all(count, N, stream_property(_, file_no(N)), Streams).
health(loadavg, LoadAVG) :-
	catch(setup_call_cleanup(
		  open('/proc/loadavg', read, In),
		  read_string(In, _, String),
		  close(In)),
	      _, fail),
	split_string(String, " ", " ", [One,Five,Fifteen|_]),
	maplist(number_string, LoadAVG, [One,Five,Fifteen]).
:- if(current_predicate(malloc_property/1)).
health(heap, json{inuse:InUse, size:Size}) :-
	malloc_property('generic.current_allocated_bytes'(InUse)),
	malloc_property('generic.heap_size'(Size)).
:- endif.
:- endif. % have library(http/http_server_health).


%!  query_form(+Request)
%
%   Provide a page for issuing a =SELECT= query.

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
              li(a(href('http://rdf4j.org/'),
                   'Sesame and SeRQL site'))
            ])).

%!  load_file_form(+Request)
%
%   Provide a form for uploading triples from a local file.

load_file_form(Request) :-
    authorized(write(default, load(posted))),
    reply_html_page(cliopatria(default),
                    title('Upload RDF'),
                    [ h1('Upload an RDF document'),

                      \explain_file_form,

                      form([ action(location_by_id(upload_data)),
                             method('POST'),
                             enctype('multipart/form-data')
                           ],
                           [ \hidden(resultFormat, html),
                             table(class(form),
                                   [tr([ th(class(label), 'File:'),
                                         td(input([ name(data),
                                                    id(filename),
                                                    type(file),
                                                    size(50)
                                                  ]))
                                       ]),
                                    tr([ th(class(label), 'Graph:'),
                                         td(input([ name(baseURI),
                                                    id(namedgraph),
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
                           ]),
                      \graph_script(Request)
                    ]).

explain_file_form -->
    html({|html||
<p>Upload RDF to the ClioPatria triple store. The uploaded file may
contain <a href="http://www.w3.org/TR/REC-rdf-syntax/">RDF/XML</a>, <a
href="http://www.w3.org/TR/turtle/">Turtle</a> or <a
href="http://www.w3.org/TR/n-triples/">ntriples</a>. The file is
processed using <a href="http://www.libarchive.org/"> libarchive</a>,
which implies it can be a (nested) archive and may optionally be
compressed. </p>

<p>
Alternatively you can use <a href="loadURL">loadURL</a> to load data from a web server.
</p>
         |}).


graph_script(Request) -->
    { http_public_host_url(Request, Host),
      http_absolute_location(root(data/uploaded), Location, []),
      string_concat(Host, Location, URL)
    },
    html_requires(jquery),
    js_script({|javascript(URL)||
$(function() {
  if ( $("#filename").val() ) {
    $("#namedgraph").val(URL+"/"+$("#filename").val());
  }

  $("#filename").on("change", function(ev) {
    var filename = $(ev.target).val();
    console.log("Changed file", filename);
    $("#namedgraph").val(URL+"/"+filename);
  });
});
              |}).


%!  load_url_form(+Request)
%
%   Provide a form for uploading triples from a URL.

load_url_form(_Request) :-
    reply_html_page(cliopatria(default),
                    title('Load RDF from HTTP server'),
                    [ h1('Load RDF from HTTP server'),

                      \explain_url_form,

                      form([ action(location_by_id(upload_url)),
                             method('GET')
                           ],
                           [ \hidden(resultFormat, html),
                             table(class(form),
                                   [tr([ th(class(label), 'URL:'),
                                         td(input([ name(url),
                                                    id(url),
                                                    value('http://'),
                                                    size(50)
                                                  ]))
                                       ]),
                                    tr([ th(class(label), 'Graph:'),
                                         td(input([ name(baseURI),
                                                    id(namedgraph),
                                                    value('http://'),
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
                           ]),
                      \url_graph_script
                    ]).


url_graph_script -->
    html_requires(jquery),
    js_script({|javascript||
$(function() {
  $("#url").on("change keyup", function(ev) {
    var url = $(ev.target).val();
    $("#namedgraph").val(url);
  });
});
              |}).


explain_url_form -->
    html({|html||

<p>Download RDF from an URL and insert it into the ClioPatria triple
store. The downloaded document may contain <a
href="http://www.w3.org/TR/REC-rdf-syntax/">RDF/XML</a>, <a
href="http://www.w3.org/TR/turtle/">Turtle</a> or <a
href="http://www.w3.org/TR/n-triples/">ntriples</a>. The file is
processed using <a href="http://www.libarchive.org/"> libarchive</a>,
which implies it can be a (nested) archive and may optionally be
compressed. </p>

<p>
Alternatively you can use <a href="loadFile">loadFile</a> to upload
a file through your browser.
</p>
         |}).

%!  load_library_rdf_form(+Request)
%
%   Provide a form  for  loading  an   ontology  from  the  library.
%   Libraries are made  available  through   the  file  search  path
%   =ontology=. Directories found through this   alias  are searched
%   recursively for files named =|Manifest.ttl|=.
%
%   @see file_search_path/2
%   @see rdf_attach_library/1.

load_library_rdf_form(Request) :-
    authorized(read(status, listBaseOntologies)),
    get_base_ontologies(Request, Ontologies),
    reply_html_page(cliopatria(default),
                    title('Load server-side RDF library'),
                    [ h1('Load a registered RDF library'),
                      p('Select a resource from the registered libraries'),
                      \load_base_ontology_form(Ontologies)
                    ]).


%!  load_base_ontology_form(+Ontologies)//
%
%   HTML component that emits a form to load a base-ontology and its
%   dependencies. Ontologies is a list   of  ontology-identifiers as
%   used by rdf_load_library/1.

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
    catch(findall(O, library_ontology(O), List0), _, fail),
    !,
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

%!  clear_repository_form(+Request)
%
%   HTTP handle presenting a form to clear the repository.

clear_repository_form(_Request) :-
    reply_html_page(cliopatria(default),
                    title('Clear triple store'),
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


%!  remove_statements_form(+Request)
%
%   HTTP handler providing a form to remove RDF statements.

remove_statements_form(_Request) :-
    reply_html_page(cliopatria(default),
                    title('Remove triples from store'),
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
