/*  Part of ClioPatria

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2012 VU University Amsterdam

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

:- module(flint, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/json)).

:- http_handler(flint('index.html'), sparql_editor, []).
:- http_handler(flint('config.js'), flint_config, []).
:- http_handler(flint(.), http_reply_from_files(flint(.), []), [prefix]).

:- html_resource(flint('config.js'),
		 [ requires([ flint('jquery-1.5.2.min.js'),
			      flint('lib/codemirror.js'),
			      flint('sparql10querymode_ll1.js'),
			      flint('sparql11querymode_ll1.js'),
			      flint('sparql11updatemode_ll1.js'),
			      flint('flint-editor.js'),

			      flint('lib/codemirror.css'),
			      flint('css/sparqlcolors.css'),
			      flint('css/docs.css')
			    ])
		 ]).


%%	sparql_editor(+Request)
%
%	HTTP handler that presents the flint SPARQL editor.

sparql_editor(_Request) :-
	reply_html_page(
	    cliopatria(plain),
	    title('Flint SPARQL Editor'),
	    \flint_page).

flint_page -->
	html_requires(flint('config.js')),
	html(div(id(flint), [])).

%%	flint_config(+Request)
%
%	HTTP handler that serves the   flint SPARQL editor configuration
%	and initialization.

flint_config(_Request) :-
	config(Config),
	format('Content-type: text/javascript~n~n'),
	format('$(document).ready(function() {~n'),
	write_config(Config),
	write_init,
	format('});~n').

write_config(Config) :-
	format('  var flintConfig = '),
	json_write(current_output, Config, [width(72)]),
	format(';').

write_init :-
	format('  var fint = new FlintEditor("flint", "images", flintConfig);~n').

%%	config(-Config) is det.
%
%	Produce a JSON document holding the FlintEditor configuration.

config(json([ interface = json([ toolbar=true,
				 menu=true
			       ]),
	      namespaces = NameSpaces,
	      defaultEndpointParameters = EndpointParameters,
	      endpoints = EndPoints,
	      defaultModes = Modes
	    ])) :-
	namespaces(NameSpaces),
	endpoint_parameters(EndpointParameters),
	endpoints(EndPoints),
	modes(Modes).

namespaces(NameSpaces) :-
	setof(NameSpace, namespace(NameSpace), NameSpaces).

namespace(json([ name(Prefix),
		 prefix(Prefix),
		 uri(URI)
	       ])) :-
	rdf_current_ns(Prefix, URI).

endpoint_parameters(
    json([ queryParameters =
	       json([ format(output),
		      query(query),
		      update(update)
		    ]),
	   selectFormats =
	     [ json([ name('Plain text'),
		      format(text),
		      type('text/plain')
		    ]),
	       json([ name('SPARQL-XML'),
		      format(sparql),
		      type('application/sparql-results+xml')
		    ]),
	       json([ name('JSON'),
		      format(json),
		      type('application/sparql-results+json')
		    ])
	     ],
	   constructFormats =
	     [ json([ name('Plain text'),
		      format(text),
		      type('text/plain')
		    ]),
	       json([ name('RDF/XML'),
		      format(rdfxml),
		      type('application/rdf+xml')
		    ]),
	       json([ name('Turtle'),
		      format(turtle),
		      type('application/turtle')
		    ])
	     ]
	 ])).


endpoints([ json([ name('ClioPatria'),
		   uri(EndPoint),
		   modes([ sparql11query, sparql11update, sparql10])
		 ])
	  ]) :-
	http_link_to_id(sparql_reply, [], EndPoint).


modes([ json([ name('SPARQL 1.1 Query'),
	       mode(sparql11query)
	     ]),
	json([ name('SPARQL 1.1 Update'),
	       mode(sparql11update)
	     ]),
	json([ name('SPARQL 1.0'),
	       mode(sparql10)
	     ])
      ]).
