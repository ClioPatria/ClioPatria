:- module(conf_store, []).
:- use_module(library(settings)).

/** <module> Configure persistent storage

Enable persistent storage of RDF graphs using the given directory.

@see library(semweb/rdf_persistency)
*/

:- set_setting_default(cliopatria:persistent_store, 'RDF-store').
:- set_setting_default(cliopatria:pre_index_tokens, true).
:- set_setting_default(cliopatria:pre_index_stems, false).

