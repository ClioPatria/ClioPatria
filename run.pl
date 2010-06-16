#!/home/janw/bin/swipl -g serql_welcome -s

:- use_module(library(semweb/rdf_db)).

:- rdf_register_ns(cornetto, 'http://purl.org/vocabularies/cornetto/').

file_search_path(ecdemo,        '../econnect/demo').
file_search_path(ontology_root, ecdemo('ClioPatria/ontologies/base')).
file_search_path(ontology_root, ecdemo('vocs')).

:- load_files([ cliopatria('applications/help/load'),
		cliopatria(load)
	      ],
	      [ silent(true),
		if(changed)
	      ]).

:- cp_server.
:- debug(http(request)).

