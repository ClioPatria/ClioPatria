/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam,
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

:- module(api_rdflib,
	  [ library_ontology/1		% -Name
	  ]).
:- use_module(user(user_db)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_library)).
:- use_module(rdfql(rdf_io)).
:- use_module(sesame).
:- use_module(components(messages)).

/** <module> Provide access to the ontology library

@see library(semweb/rdf_library)
*/

:- http_handler(sesame('loadLibraryOntology'),   load_library_ontology,
		[time_limit(infinite)]).
:- http_handler(sesame('listLibraryOntologies'), list_library_ontologies, []).

%%	load_library_ontology(+Request)
%
%	Load a named ontology from the ontology library.
%
%	@tbd	Cannot use concurrent loading as the load as a whole is
%		inside an rdf transaction.

load_library_ontology(Request) :-
	http_parameters(Request,
			[ repository(Repository),
			  ontology(Ontology, []),
			  resultFormat(Format)
			],
			[ attribute_declarations(attribute_decl)
			]),
	authorized(write(Repository, load(library_ontology(Ontology)))),
	prepare_ontology_library,
	api_action(Request,
		   rdf_load_library(Ontology, []),
		   Format,
		   \loaded_library_ontology(Ontology)).

loaded_library_ontology(Id) -->
	html('Loaded base ontology '),
	(   { rdf_library_index(Id, title(Title)) }
	->  html([Id, ' -- ', Title])
	;   html(Id)
	).


%%	list_library_ontologies(+Request)
%
%	Reply with a list of available base ontologies

list_library_ontologies(Request) :-
	authorized(read(status, listBaseOntologies)),
	http_parameters(Request,
			[ resultFormat(Format),
			  serialization(Serialization)
			],
			[ attribute_declarations(attribute_decl)
			]),
	catch(findall(row(O), library_ontology(O), Rows0), _,
	      Rows0 = []),
	sort(Rows0, Rows),
	write_table(Rows,
		    [ result_format(Format),
		      serialization(Serialization),
		      variables(varnames(ontology))
		    ]).


%%	library_ontology(-Name) is nondet.
%
%	True if Name is the name of an ontology from the library.

library_ontology(O) :-
	prepare_ontology_library,
	rdf_library_index(O, title(_Title)).


%%	prepare_ontology_library is det.
%
%	Load RDF library manifests from   directories  defined using the
%	file_search_path/2 =ontology= alias.

prepare_ontology_library :-
	(   absolute_file_name(rdf(.), Dir,
			       [ file_type(directory),
				 solutions(all)
			       ]),
	    rdf_attach_library(Dir),
	    fail
	;   true
	).

%%	attribute_decl(+Name, -Options)
%
%	Copied from Sesame

attribute_decl(repository,
	       [ optional(true),
		 description('Name of the repository (ignored)')
	       ]).
attribute_decl(resultFormat,
	       [ default(xml),
		 type(oneof([ xml,
			      html,
			      rdf
			    ])),
		 description('Serialization format of the result')
	       ]).
attribute_decl(ontology,
	       [ description('Name of the ontology to load')
	       ]).
