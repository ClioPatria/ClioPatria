:- module(conf_ontopath, []).

/** <module> Configure location of the ontology (RDF) library

This module uses user:file_search_path/2 to define locations for loading
RDF data. Ontology library directories   contain  files =|Manifest.ttl|=
that describe RDF-data and their dependencies.

Registered bundles of RDF-data that have   a dcterms:title attribute are
displayed on the page  that  appears   from  the  menu-item  =*File/Load
ontology from library*=. Manifests often specify   local files, but they
can also specify RDF from HTTP servers.

@see	rdf_attach_library/1
*/

:- multifile
	user:file_search_path/2.
:- dynamic
	user:file_search_path/2.

user:file_search_path(ontology, '../econnect/demo/vocs').
user:file_search_path(ontology, '../econnect/demo/metadata').


