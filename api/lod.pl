/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Michiel Hildebrand
    Author:        Jan Wielemaker
    E-mail:        michielh@few.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, VU University Amsterdam

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

:- module(api_lod, []).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_request_value)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_json)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_bounded)).
:- use_module(library(settings)).
:- use_module(library(rdf_write)).
:- use_module(library(uri)).

:- use_module(applications(browse)).


:- setting(lod:prefix, atom, '.',
	   'URL prefix that triggers the lod http handler').
:- setting(lod:redirect, boolean, false,
	   'Toggle the use of 303 redirects').
:- setting(lod:global_host, atom, '',
	   'Global hostname used to rewrite local requests').


:- setting(lod:prefix, Prefix),
   http_handler(root(Prefix), lod_api, [prefix]).


%%	lod_api(+Request)
%
%	Perform queries on Resource and output result in JSON (by
%	default, or other format if requested explictly in the http
%	accept header).

lod_api(Request) :-
	request_uri_components(Request, URIComponents),
	(   memberchk(accept(AcceptHeader), Request)
	->  http_parse_header_value(accept, AcceptHeader, AcceptList)
	;   AcceptList = []
	),
	lod_request(URIComponents, AcceptList, Request).

lod_request(URIComponents, AcceptList, Request) :-   % reply 303 See Other
	setting(lod:redirect, true),
	redirect(URIComponents, AcceptList, SeeOther), !,
	http_redirect(see_other, SeeOther, Request).
lod_request(URIComponents, _AcceptList, _Request) :- % reply 200 OK
	format_request(URIComponents, URI, Format), !,
	lod_describe(Format, URI).
lod_request(URIComponents, AcceptList, _Request) :-  % reply 200 OK
	uri_request(URIComponents, URI), !,
	preferred_format(AcceptList, Format),
	lod_describe(Format, URI).
lod_request(URIComponents, _AcceptList, _Request) :- % reply 404 Not Found
	uri_components(URI, URIComponents),
	throw(http_reply(not_found(URI))).


%%	request_uri_components(+Request, -URIComponents)
%
%	URIComponents contains all element in Request that together
%	create the request URL.
%
%	@see	uri_components/2 and uri_data/3 for the description of
%		URIComponents

request_uri_components(Request, Components) :-
	memberchk(request_uri(ReqURI), Request),
	http_current_host(Request, Host, Port,
			  [ global(true)
			  ]),
	uri_components(ReqURI, Components),
	uri_authority_data(host, Auth, Host),
	uri_authority_data(port, Auth, Port),
	uri_authority_components(Authority, Auth),
	uri_data(authority, Components, Authority).


%%	redirect(+URIComponents, +AcceptList, -RedirectURL)
%
%	Succeeds if URI is in the store and a RedirectURL is found for
%	it.

redirect(URIComponents, AcceptList, To) :-
	uri_components(URI, URIComponents),
	rdf_subject(URI), !,
	preferred_format(AcceptList, Format),
	(   cliopatria:redirect_url(Format, URIComponents, To)
	->  true
	;   uri_data(path, URIComponents, Path0),
	    format_suffix(Format, Suffix),
	    file_name_extension(Path0, Suffix, Path),
	    uri_data(path, URIComponents, Path, ToComponents),
	    uri_components(To, ToComponents)
	).


%%	preferred_format(+AcceptList, -Format) is det.
%
%	Format is the highest ranked mimetype found in the Acceptlist of
%	the request and that  we  can   support.  Expects  an AcceptList
%	sorted by rank.

preferred_format(AcceptList, Format) :-
	member(media(MimeType,_,_,_), AcceptList),
	ground(MimeType),
	mimetype_format(MimeType, Format), !.
preferred_format(_, html).


%%	format_request(+URIComponents, -URI, -Format) is semidet.
%
%	True if URIComponents contains a suffix   that  corresponds to a
%	supported output format, and  the  global   URI  occurs  in  the
%	database.

format_request(URIComponents, URI, Format) :-
	uri_data(path, URIComponents, Path),
	file_name_extension(Base, Ext, Path),
	(   format_suffix(Format, Ext),
	    mimetype_format(_, Format)
	->  true
	),
	uri_data(path, URIComponents, Base, PlainComponents),
	uri_components(URI, PlainComponents),
	rdf_subject(URI).

%%	uri_request(+URIComponents, -URI) is semidet.
%
%	Succeeds when the global URI corresponding with URIComponents
%	occurs in the database.

uri_request(URIComponents, URI) :-
	uri_components(URI, URIComponents),
	rdf_subject(URI).


%%	lod_describe(+Format, +URI) is det.
%
%	Write an HTTP document  describing  URI   to  in  Format  to the
%	current output. Format is defined by mimetype_format/2.

lod_describe(html, URI) :- !,
	rdf_display_label(URI, Label),
	reply_html_page(cliopatria(default),
			title('Resource ~w'-[Label]),
			\list_resource(URI, [])).
lod_describe(Format, URI) :-
	lod_description(URI, RDF),
	send_graph(Format, RDF).

send_graph(xmlrdf, RDF) :-
	format('Content-type: application/rdf+xml; charset=UTF-8~n~n'),
	rdf_write_xml(current_output, RDF).
send_graph(json, RDF) :-
	graph_json(RDF, JSON),
	reply_json(JSON).


%%	lod_description(+URI, -RDF) is det.
%
%	RDF is a  graph  represented  as   a  list  of  rdf(S,P,O)  that
%	describes URI.
%
%	This predicate is hooked   by  cliopatria:lod_description/2. The
%	default is implemented by graph_CBD/3.
%
%	@see SPARQL DESCRIBE

lod_description(URI, RDF) :-
	cliopatria:lod_description(URI, RDF), !.
lod_description(URI, RDF) :-
	graph_CBD(rdf, URI, RDF).


%%	mimetype_format(?MimeType, ?Format) is nondet.
%
%	Conversion between mimetypes and formats.

mimetype_format(application/'rdf+xml', xmlrdf).
mimetype_format(application/json,      json).
mimetype_format(text/html,	       html).

%%	format_suffix(?Format, ?Suffix) is nondet.
%
%	Suffix is the file name extension used for Format.

format_suffix(xmlrdf, rdf).
format_suffix(json,   json).
format_suffix(html,   html).


		 /*******************************
		 *	       HOOKS		*
		 *******************************/

:- multifile
	cliopatria:redirect_url/3,
	cliopatria:uri_describe/2.

%%	cliopatria:redirect_url(+Format, +URLComponents, -RedirectURL)
%
%	Compose a RedirectionURL based on the output Format and the
%	URLComponents.
%
%       @see This hook is used by redirct/3.


%%	cliopatria:uri_describe(+URI, -RDF:list(rdf(s,p,o)))
%
%	RDF is list of triples describing URI.
%
%	@see This hook is used by uri_describe/2
