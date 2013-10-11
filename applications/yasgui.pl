/*  Part of ClioPatria

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013 VU University Amsterdam

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

:- module(yasgui, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/json)).
:- use_module(library(settings)).
:- use_module(library(http/http_host)).

:- http_handler(yasgui('index.html'), yasgui_editor, []).

:- setting(server, uri, 'http://yasgui.laurensrietveld.nl',
	   'YASGUI server to use').

%%	yasgui_editor(+Request)
%
%	HTTP handler that presents the YASGUI SPARQL editor.

yasgui_editor(Request) :-
	yasgui_src_url(Request, Src),
	reply_html_page(
	    cliopatria(plain),
	    title('YASGUI SPARQL Editor'),
	    \yasgui_page(Src)).

yasgui_page(Src) -->
	html(iframe([ class(yasgui),
		      src(Src),
		      width('100%'),
		      height('100%'),
		      frameborder(0)
		    ], [])),
	js_script({|javascript||
		   window.onload=function(){
		     document.body.style.height="100%";
                     document.getElementsByTagName("html")[0].style.height="100%";
		     document.getElementById("cp-content").style.height="90%";
		   };
		   |}).


yasgui_src_url(Request, Src) :-
	http_link_to_id(sparql_query, [], SparqlLocation),
	public_url(Request, SparqlLocation, EndPoint),
	yasgui_config(EndPoint, Config),
	setting(server, YASGuiServer),
	uri_query_components(Query, [jsonSettings=Config]),
	format(atom(Src), '~w?~w', [YASGuiServer, Query]).


yasgui_config(EndPoint, Config) :-
	JSON = json([ defaults = json([ endpoint = EndPoint
				      ]),
		      singleEndpointMode = @(true),
		      defaultBookMarks = [ json([ title = 'Get 10 triples',
						  endpoint = '',
						  query = 'SELECT * WHERE {\n\c
							   ?s ?p ?o\n\c
							   } LIMIT 10'
						])
					 ]
		    ]),
	atom_json_term(Config, JSON, [as(atom)]).


%%	public_url(+Request, +Location, -URL) is det.
%
%	True when URL is an  absolute  URL   for  the  Location  on this
%	server.

public_url(Request, Location, URL) :-
	http_current_host(Request, Host, Port, [global(true)]),
	setting(http:public_scheme, Scheme),
	(   scheme_port(Scheme, Port)
	->  format(atom(URL), '~w://~w~w', [Scheme, Host, Location])
	;   format(atom(URL), '~w://~w:~w~w', [Scheme, Host, Port, Location])
	).

scheme_port(http, 80).
scheme_port(https, 443).


