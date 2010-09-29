:- module(lod_server, []).

user:file_search_path(library, cliopatria(lib)).

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
:- use_module(library(settings)).
:- use_module(library(url)).

:- use_module(rdf_json).
:- use_module(rdf_html).

:- setting(lod:prefix, atom, '.',
	   'URL prefix that triggers the lod http handler').
:- setting(lod:redirect, boolean, false,
	   'Toggle the use of 303 redirects').
:- setting(lod:global_host, atom, '',
	   'Global hostname used to rewrite local requests').


:- setting(lod:prefix, Prefix),
   http_handler(root(Prefix), lod_api, [prefix]).


/***************************************************
* API
***************************************************/

%%	lod_api(+Request)
%
%	Perform queries on Resource and output result in JSON (by
%	default, or other format if requested explictly in the http
%	accept header).

lod_api(Request) :-
	request_url_components(Request, URLComponents),
	(   memberchk(accept(AcceptHeader), Request)
	->  http_parse_header_value(accept, AcceptHeader, AcceptList)
	;   AcceptList = []
	),
	lod_request(URLComponents, AcceptList).

lod_request(URLComponents, AcceptList) :-  % reply 303
	setting(lod:redirect, true),
	redirect(URLComponents, AcceptList, SeeOther),
	!,
	throw(http_reply(see_other(SeeOther))).
lod_request(URLComponents, _AcceptList) :- % reply 200
	file_request(URLComponents, URI, Format),
	!,
	uri_describe(URI, RDF),
	lod_format_reply(Format, URI, RDF).
lod_request(URLComponents, AcceptList) :- % reply 200
	uri_request(URLComponents, URI),
	!,
	uri_describe(URI, RDF),
	lod_reply(AcceptList, URI, RDF).
lod_request(URLComponents, _AcceptList) :- % reply 404
 	 parse_url(URL, URLComponents),
	 throw(http_reply(not_found(URL))).


%%	request_url_components(+Request, -URLComponents)
%
%	URLComponents contains all element in Request that together
%	create the request URL.

request_url_components(Request, [ protocol(http),
				  host(Host), port(Port),
				  path(Path), search(Search)
				]) :-
	http_current_host(Request, Host, Port,
			  [ global(false)
			  ]),
 	(   option(x_redirected_path(Path), Request)
	->  true
	;   option(path(Path), Request, /)
	),
	option(search(Search), Request, []).

%%	redirect(+URLComponents, +AcceptList, -RedirectURL)
%
%	Succeeds if URI is in the store and a RedirectURL is found for
%	it.

redirect(URLComponents, AcceptList, To) :-
 	url_to_uri(URLComponents, URI),
	rdf_subject(URI), !,
	preferred_format(AcceptList, Format),
	(   catch(cliopatria:redirect_url(Format, URLComponents, To), _, fail)
	->  true
	;   select(path(Path0), URLComponents, Cs),
	    format_suffix(Format, Suffix),
	    atom_concat(Path0, Suffix, Path),
	    parse_url(To, [path(Path)|Cs])
	).


%%	preferred_format(+AcceptList, -Format)
%
%	Format is the highest ranked mimetype found in the
%	Acceptlist of the request and that we can support. Expects an
%	AcceptList sorted by rank.

preferred_format(AcceptList, Format) :-
	member(media(MimeType,_,_,_), AcceptList),
	ground(MimeType),
	mimetype_format(MimeType, Format),
	!.
preferred_format(_, html).


%%	file_request(+URLComponents, -URI, -Format)
%
%	Succeeds when URL contains a suffix that corresponds to a
%	supported output format, and the global URI occurs in the
%	database.

file_request(URLComponents, URI, Format) :-
	select(path(Path0), URLComponents, Cs0),
	select(search(_), Cs0, Cs),
	mimetype_format(_MimeType, Format),
	format_suffix(Format, Suffix),
	atom_concat(Path, Suffix, Path0),
	url_to_uri([path(Path)|Cs], URI),
	rdf_subject(URI).

%%	uri_request(+URLComponents, -URI)
%
%	Succeeds when the global uri corresponding with URLComponents
%	occurs in the database.

uri_request(URLComponents, URI) :-
	 url_to_uri(URLComponents, URI),
	 rdf_subject(URI).


%%	lod_reply(+AcceptList, +URI, +RDF)
%
%	Reply RDF graph using the preferred format from AcceptList.

lod_reply(AcceptList, URI, RDF) :-
        preferred_format(AcceptList, Format),
	lod_format_reply(Format, URI, RDF).

%%	lod_format_reply(+Format, +URI, +RDF)
%
%	Reply RDF graph formatted according to Format.

lod_format_reply(xmlrdf, _URI, RDF) :- !,
	format('Content-type: application/rdf+xml; charset=UTF-8~n~n'),
	rdf_write_xml(current_output, RDF).
lod_format_reply(json, URI, RDF) :- !,
	rdf_subject_pv(RDF, URI, Grouped),
	properties_to_json(Grouped, JSON),
	reply_json(json([URI=json(JSON)])).
lod_format_reply(html, URI, RDF) :-
	rdf_write:used_namespaces(RDF, NSList),
	namespace_decl(NSList, NSDecl),
	rdf_subject_pv(RDF, URI, Grouped),
	rdfs_ns_label(URI, Label),
	reply_html_page(title('LOD --- ~w'-[Label]),
 			[ div(NSDecl,
			      [ h1(class(title),
				   a([about(URI),href(URI)], Label)),
				\property_table(Grouped)
			      ])
			]).

%%	namespace_decl(+Namespaces, -Declarations)
%
%	Namespace declarations.

namespace_decl([], []).
namespace_decl([NS|T], [xmlns:NS=Full|Rest]) :-
	rdf_db:ns(NS, Full),
 	namespace_decl(T, Rest).

%%	rdf_subject_pv(+RDF, +URI, -Grouped)
%
%	Grouped is a list of all property-values pairs for URI.
%	Values is a list of all values.

rdf_subject_pv(RDF, URI, Grouped) :-
	findall(P-V, member(rdf(URI,P,V),RDF), Pairs0),
 	sort(Pairs0, Pairs),
	group_pairs_by_key(Pairs, Grouped).


%%	uri_describe(+URI, -RDF)
%
%	RDF describes URI.

uri_describe(URI, RDF) :-
	catch(cliopatria:uri_describe(URI, RDF), _, fail),
	!.
uri_describe(URI, RDF) :-
	findall(rdf(URI,P,O), rdf(URI,P,O), RDF).


%%	url_to_uri(+URLComponents, -URI)
%
%	Translate between the URLComponents from an http request and a
%	global URI.

url_to_uri(URLComponents, URI) :-
	catch(cliopatria:url_to_uri(URLComponents, URI), _, fail),
	!.
url_to_uri(URLComponents, URI) :-
	setting(lod:global_host, Host),
	Host \== '',
	!,
	select(path(Path0), URLComponents, Cs1),
	setting(http:prefix, Prefix),
	atom_concat(Prefix, Path, Path0),
	delete(Cs1, host(_), Cs2),
	delete(Cs2, port(_), Cs), % we assume the port is not needed anymore
	parse_url(URI, [host(Host),path(Path)|Cs]).
url_to_uri(URLComponents, URI) :-
	parse_url(URI, URLComponents).


%%	mimetype_format(?MimeType, ?Extension)
%
%	Conversion between mimetypes and formats.

mimetype_format(application/'rdf+xml', xmlrdf).
mimetype_format(application/json, json).
mimetype_format(text/html, html).

%%	format_suffix(?Format, ?Suffix)
%
%	Conversion between formats and url suffixes

format_suffix(xmlrdf, '.rdf').
format_suffix(json, '.json').
format_suffix(html, '.html').


/***************************************************
* Hooks
***************************************************/

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


%%	cliopatria:url_to_uri(+URLComponents, -URI)
%
%	URI is a global uri corresponding to the URLComponents from the
%	http request.
%
%	@see This hook is used by url_to_uri
