:- module(conf_lod, []).
:- use_module(api(lod)).
:- use_module(library(http/http_dispatch)).

/** <module> Configure Linked Data (LOD) access

Load the linked-data server and the   library to register HTTP handlers.
and then register your LOD areas and/or  handlers for locations that are
redirected from e.g., http://www.purl.org. Multiple   handlers can point
to lod_api/1, but one handler should not be  a prefix of another one (as
in /rdf/ and /rdf/time/). The first   example  assumes that requests for
RDF URIs arrive at this server directly   or through a proxy. The latter
assumes that /mydata/ on purl.org is   redirected  to /purl/rdf/ on this
server and all RDF URIs start with http://www.purl.org/mydata/

@see cliopatria(api/lod)
*/

%:- http_handler('/rdf/', lod_api, [prefix]).
%:- http_handler('/purl/rdf/', lod_api,
%		[ redirected_for('http://www.purl.org/mydata/'),
%		  prefix
%		]).

