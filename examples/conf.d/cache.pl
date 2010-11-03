:- module(conf_cache, []).
:- use_module(library(semweb/rdf_cache)).

/** <module> Configure caching of RDF inputs

Configure caching of RDF inputs. If enabled, RDF input graphs are stored
in fast-load format in the given cache   directory. This provides a huge
speedup for loading RDF into the database.   Note that if persistency is
enabled (default), RDF data is  stored   efficiently  in  the files that
realise the persistent RDF database  and   enabling  caching  only makes
sense if the persistent database is  frequently wiped and re-filled with
(partially) the same data.

Unloading this plugin  does  _not_   disable  caching.  Instead,  change
*enabled* to =false=.
*/

:- rdf_set_cache_options([ enabled(true),
			   global_directory('cache/rdf'),
			   create_global_directory(true)
			 ]).

