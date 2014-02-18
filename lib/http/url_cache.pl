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

:- module(url_cache,
	  [ url_cache/3,		% +URI, -File, -MimeType
	    url_cache_file/4,		% +URL, +Dir, +Ext, -Path)
	    url_cache_delete/1,		% +URI
	    url_cached/2,		% ?URL, ?Property
	    url_cached/3,		% +Dir, ?URL, ?Property
	    url_cache_reset_server_status/0,
	    url_cache_reset_server_status/1 % +Server
	  ]).
:- use_module(library(http/http_open)).
:- if(exists_source(library(http/http_ssl_plugin))).
:- use_module(library(http/http_ssl_plugin)).
:- endif.
:- use_module(library(http/mimetype)).
:- use_module(library(url)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(settings)).
:- use_module(library(base64)).
:- use_module(library(utf8)).
:- use_module(library(lists)).
:- use_module(library(sha)).

:- setting(cache:url_cache_directory, atom, 'cache/url',
	   'Directory to cache fetched remote URLs').

/** <module> Cache the content of external URLs in local files

This library provides a cache  for  data   stored  in  extenal URLs. The
content of each URL is kept in a  file and described by a meta-file that
remembers the mime-type, the original URL, when   it was fetched and -if
provided by the server- the last-modified stamp.

@tbd	The current implementation does not validate the cache content, nor
	does it honour the HTTP cache directives.
*/


%%	url_cache(+URI:atom, -Path:atom, -MimeType:atom) is det.
%
%	Return the content of URI in  a   file  at Path. MimeType is the
%	Mime-type returned by the server.
%
%	@error	existence_error(url, URL)
%		Server did not respond with 200 OK
%	@error  existence_error(source_sink, url_cache(.))
%		Cache directory does not exist
%	@bug	Does not check modification time and cache validity

url_cache(URL, Path, MimeType) :-
	url_cache_dir(Dir),
	url_cache_file(URL, Dir, url, Path),
	atom_concat(Path, '.meta', TypeFile),
	(   exists_file(Path),
	    exists_file(TypeFile),
	    read_meta_file(TypeFile, mime_type(MimeType0))
	->  MimeType = MimeType0
	;   fetch_url(URL, Path, MimeType, Modified),
	    get_time(NowF),
	    Now is round(NowF),
	    open(TypeFile, write, Out,
		 [ encoding(utf8),
		   lock(write)
		 ]),
	    format(Out,
		   'mime_type(~q).~n\c
		    url(~q).~n\c
		    fetched(~q).~n',
		   [MimeType, URL, Now]),
	    (	nonvar(Modified)
	    ->	format(Out, 'last_modified(~q).~n', [Modified])
	    ;	true
	    ),
	    close(Out)
	).

read_meta_file(MimeFile, Term) :-
	setup_call_cleanup(open(MimeFile, read, In,
				[ encoding(utf8),
				  lock(read)
				]),
			   ndet_read(In, Term),
			   close(In)).

ndet_read(Stream, Term) :-
	repeat,
	read(Stream, Term0),
	(   Term0 == end_of_file
	->  !, fail
	;   Term = Term0
	).

%%	url_cache_delete(+URL) is det.
%
%	Delete an URL from the cache. Succeeds,  even if the cache files
%	do not exist.
%
%	@error	Throws exceptions from delete_file/1 other than
%		existence errors.

url_cache_delete(URL) :-
	url_cache_dir(Dir),
	url_cache_file(URL, Dir, url, Path),
	atom_concat(Path, '.meta', TypeFile),
	catch(delete_file(TypeFile), E0, true),
	catch(delete_file(Path), E1, true),
	error_ok(E0),
	error_ok(E1).

error_ok(E) :-
	subsumes_term(error(existence_error(file, _), _), E), !.
error_ok(E) :-
	throw(E).

%%	url_cache_dir(-Dir) is det
%
%	Return or create the URL caching directory

url_cache_dir(Dir) :-
	setting(cache:url_cache_directory, Dir),
	make_directory_path(Dir).

%%	make_directory_path(+Dir) is det.
%
%	Create Dir and all required components.

make_directory_path(Dir) :-
	make_directory_path_2(Dir), !.
make_directory_path(Dir) :-
	permission_error(create, directory, Dir).

make_directory_path_2(Dir) :-
	exists_directory(Dir), !.
make_directory_path_2(Dir) :-
	Dir \== (/), !,
	file_directory_name(Dir, Parent),
	make_directory_path_2(Parent),
	make_directory(Dir).

%%	fetch_url(+URL:atom, +Path:atom, -MimeType:atom) is det.
%
%	@error	existence_error(url, URL)

fetch_url(URL, File, MimeType, Modified) :-
	parse_url_ex(URL, Parts),
	server(Parts, Server),
	(   allow(Server)
	->  true
	;   throw(error(existence_error(url, URL),
			context(url_cache/3, 'Too many errors from server')))
	),
	get_time(Now),
	(   catch(fetch_url_raw(URL, File,
				MimeType, Modified), E, true)
	->  (   var(E)
	    ->	register_stats(Server, Now, true)
	    ;	register_stats(Server, Now, error(E)),
		throw(E)
	    )
	;   register_stats(Server, Now, false)
	).

server(Parts, Server) :-
	memberchk(host(Host), Parts), !,
	(   memberchk(port(Port), Parts)
	->  Server = Host:Port
	;   Server = Host
	).
server(_,_) :-
	assertion(false).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Server status assessment. We keep a   health-status  of the server using
the following rules:

    * Range -100 .. 100
    * Ok if > 0
    * The initial status is 100 (healthy)
    * Possitive results add 20-4*sqrt(Time)
    * Negative results subtract 10
    * Add 1 per minute since last status.

TBD: frequency matters: requests should not pile up.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	server_status/3.		% Server, Status, Last

allow(Server) :-
	server_status(Server, Status),
	debug(url_cache, 'Status ~q: ~w', [Server, Status]),
	Status > 0.

server_status(Server, Status) :-
	get_time(Now),
	with_mutex(url_cache_status,
		   server_status(Server, S0, T0)), !,
	Status is min(100, S0 + round(Now-T0)//60).
server_status(_, 100).

register_stats(Server, Start, Result) :-
	get_time(Now),
	Time is Now - Start,
	(   server_status(Server, S0, T0)
	->  true
	;   S0 = 100,
	    T0 = Now
	),
	Since is Start - T0,
	update_status(Result, Time, Since, S0, S1),
	with_mutex(url_cache_status,
		   (   retractall(server_status(Server, _, _)),
		       assert(server_status(Server, S1, Start)))).

update_status(true, Time, Since, S0, S) :- !,
	S is min(100, S0 + round(20-4*sqrt(Time)) + round(Since)//60).
update_status(_, Time, _Since, S0, S) :- !,
	S is max(-100, S0 - (10 + round(Time))).


%%	url_cache_reset_server_status is det.
%%	url_cache_reset_server_status(+Server) is det.
%
%	Reset the status of the given server or all servers.

url_cache_reset_server_status :-
	with_mutex(url_cache_status,
		   retractall(server_status(_,_,_))).
url_cache_reset_server_status(Server) :-
	must_be(atom, Server),
	with_mutex(url_cache_status,
		   retractall(server_status(Server,_,_))).


%%	fetch_url_raw(+URL:atom, +Path:atom, -MimeType:atom, -Modified) is det.
%
%	Fetch data from URL and put it   into the file Path. MimeType is
%	unified  with  the  MIME-type  as  reported  by  the  server  or
%	text/plain if the server did not provide a MIME-Type.
%
%	@error	existence_error(url, URL)

fetch_url_raw(URL, File, MimeType, Modified) :-
	debug(url_cache, 'Downloading ~w ...', [URL]),
	atom_concat(File, '.tmp', TmpFile),
	(   catch(fetch_to_file(URL, TmpFile, Code, Header), E, true)
	->  true
	;   E = predicate_failed(http_get/3)
	),
	(   var(E)
	->  true
	;   (   debugging(url_cache)
	    ->	print_message(error, E)
	    ;	true
	    ),
	    catch(delete_file(TmpFile), _, true),
	    (	debugging(url_cache)
	    ->	message_to_string(E, Msg),
		debug(url_cache, 'Download failed: ~w', [Msg])
	    ;	true
	    ),
	    throw(E)
	),
	(   Code == 200
	->  rename_file(TmpFile, File)
	;   catch(delete_file(TmpFile), _, true),
	    throw(error(existence_error(url, URL), _))
	),
	(   memberchk(content_type(MimeType0), Header)
	->  true
	;   MimeType0 = 'text/plain'
	),
	ignore(memberchk(last_modified(Modified), Header)),
	debug(url_cache, 'Downloaded ~w, mime-type: ~w',
	      [URL, MimeType0]),
	MimeType = MimeType0.

fetch_to_file(URL, File, Code,
	      [ content_type(ContentType),
		last_modified(LastModified)
	      ]) :-
	setup_call_cleanup(
	    open(File, write, Out, [ type(binary) ]),
	    setup_call_cleanup(
		http_open(URL, In,
			  [ header(content_type, ContentType),
			    header(last_modified, LastModified),
			    status_code(Code),
			    cert_verify_hook(ssl_verify)
			  ]),
		copy_stream_data(In, Out),
		close(In)),
	    close(Out)).

:- public ssl_verify/5.

%%	ssl_verify(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Error)
%
%	Currently we accept  all  certificates.

ssl_verify(_SSL,
	   _ProblemCertificate, _AllCertificates, _FirstCertificate,
	   _Error).

parse_url_ex(URL, Parts) :-
	is_list(URL), !,
	Parts = URL.
parse_url_ex(URL, Parts) :-
	parse_url(URL, Parts), !.
parse_url_ex(URL, _) :-
	domain_error(url, URL).

%%	url_cache_file(+URL, +Dir, +Ext, -Path) is det
%
%	Determine location of cache-file for the   given  URL in Dir. If
%	Ext is provided, the  returned  Path   is  ensured  to  have the
%	specified extension.

url_cache_file(URL, Dir, Ext, Path) :-
	url_to_file(URL, Ext, File),
	sub_atom(File, 0, 2, _, L1),
	ensure_dir(Dir, L1, Dir1),
	sub_atom(File, 2, 2, _, L2),
	ensure_dir(Dir1, L2, Dir2),
	sub_atom(File, 4, _, 0, LocalFile),
	atomic_list_concat([Dir2, /, LocalFile], Path).

ensure_dir(D0, Sub, Dir) :-
	atomic_list_concat([D0, /, Sub], Dir),
	(   exists_directory(Dir)
	->  true
	;   make_directory(Dir)
	).

%%	url_to_file(+URL, +Ext, -File) is det.
%
%	File is a filename for storing URL and has extension Ext. We use
%	a cryptographic hash to ensure consistent naming, a name that is
%	guaranteed to fit in every sensible filesystem and ensure a good
%	distribution of the cache directories.

url_to_file(URL, Ext, File) :-
	sha_hash(URL, Hash, []),
	phrase(hex_digits(Hash), Codes),
	string_to_list(String, Codes),
	file_name_extension(String, Ext, File).

hex_digits([]) -->
	"".
hex_digits([H|T]) -->
	byte(H),
	hex_digits(T).

byte(Byte) -->
	{ High is (Byte>>4) /\ 0xf,
	  Low is (Byte /\ 0xf),
	  code_type(H, xdigit(High)),
	  code_type(L, xdigit(Low))
	},
	[H,L].


		 /*******************************
		 *	    READ CACHE		*
		 *******************************/

%%	url_cached(?URL, ?Property) is nondet.
%%	url_cached(+Dir, ?URL, ?Property) is nondet.
%
%	True if URL is in the cache represented by the directory Dir and
%	has Property.  Defined properties are:
%
%	    * file(-File)
%	    File is the cache-file for the given URL
%	    * mime_type(-Mime)
%	    Mime is the mime-type of the URL as reported by the server
%	    * fetched(-Stamp:integer)
%	    Timestamp that specifies when the URL was fetched
%	    * last_modified(-Modified:atom)
%	    If present, this is the modification time as provided by
%	    the server.

url_cached(URL, Property) :-
	url_cache_dir(Dir),
	url_cached(Dir, URL, Property).

url_cached(Dir, URL, Property) :-
	nonvar(URL), !,
	url_cache_file(URL, Dir, url, Path),
	atom_concat(Path, '.meta', MetaFile),
	exists_file(MetaFile),
	cache_file_property(Property, MetaFile).
url_cached(Dir, URL, Property) :-
	nonvar(Property),
	Property = file(File),
	atom(File),
	atom_concat(Dir, Rest, File),
	\+ sub_atom(Rest, _, _, _, '../'),
	file_name_extension(Base, url, File),
	file_name_extension(Base, meta, MetaFile),
	exists_file(MetaFile),
	once(read_meta_file(MetaFile, url(URL))).
url_cached(Dir, URL, Property) :-
	atom_concat(Dir, '/??', TopPat),
	expand_file_name(TopPat, TopDirs),
	member(TopDir, TopDirs),
	atom_concat(TopDir, '/??', DirPat),
	expand_file_name(DirPat, FileDirs),
	member(FileDir, FileDirs),
	atom_concat(FileDir, '/*.meta', FilePat),
	expand_file_name(FilePat, MetaFiles),
	member(MetaFile, MetaFiles),
	once(read_meta_file(MetaFile, url(URL))),
	check_cache_file(MetaFile, URL),
	cache_file_property(Property, MetaFile).

check_cache_file(MetaFile, URL) :-
	file_name_extension(File, meta, MetaFile),
	(   exists_file(File)
	->  true
	;   print_message(warning, url_cache(no_file(File, MetaFile, URL))),
	    delete_file(MetaFile),
	    fail
	).

cache_file_property(Property, MetaFile) :-
	var(Property), !,
	cache_file_property_ndet(Property, MetaFile).
cache_file_property(Property, MetaFile) :-
	cache_file_property_ndet(Property, MetaFile), !.


cache_file_property_ndet(file(File), MetaFile) :-
	file_name_extension(File, meta, MetaFile).
cache_file_property_ndet(P, MetaFile) :-
	read_meta_file(MetaFile, P),
	P \= url(_).

		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile
	prolog:message//1.

prolog:message(url_cache(no_file(File, _MetaFile, URL))) -->
	[ 'URL Cache: file ~q does not exist (URL=~q)'-[File, URL] ].
