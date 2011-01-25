:- module(conf_zlib, []).

/** <module> Enable the use of compressed RDF data

After loading this configuration file, rdf_load/1   and friends can load
gzipped  (=|.gz|=)  RDF  files  (both    RDF/XML   and  Turtle)  without
decompressing them first.

This module is not enabled by default because the SWI-Prolog zlib module
may not be available if zlib is not installed on the computer.
*/

:- use_module(library(semweb/rdf_zlib_plugin)).

