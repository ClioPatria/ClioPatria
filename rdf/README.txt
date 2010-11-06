---+ Provided ontologies

This directory contains ontologies that are distributed with ClioPatria.
The directory is  added  to  the   ontology-repository  by  means  of  a
user:file_search_path/2 declaration for the alias =ontology_root= in the
file cliopatria(parms). Meta-information  about  the   provided  in  the
file(s) Manifest.ttl.

Ontologies can be loaded into ClioPatria by  means of the menu File/Load
base ontology.

  * The sub-directory =base= contains core ontologies that are required
  in most domains where we see a role for ClioPatria.

  * The sub-directory =tool= contains ontologies that drive parts of
  ClioPatria. E.g., =|graphviz.ttl|= provides the vocabulary to
  customize (context) graphs.

@see library(semweb/rdf_library)
