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

:- module(http_canviz,
	  [ canviz_graph//2		% :Closure, +Options
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(process)).
:- use_module(library(debug)).
:- use_module(rdf_graphviz).

:- html_resource(js('canviz.js'),
		 [ requires([ js('path/path.js'),
			      js('prototype/prototype.js')
			    ])
		 ]).
:- html_resource(js('path/path.js'),
		 [ requires([ js('prototype/prototype.js')
			    ])
		 ]).

:- http_handler(root(canviz_send_graph), send_graph, []).

%%	canviz_graph(:Closure, +Options)//
%
%	Display an RDF graph on a canviz (HTML5). The graph is a list of
%	rdf(S,P,O) triples and is  obtained   by  calling  call(Closure,
%	Graph). Options is an option-list for gviz_write_rdf/3.

:- meta_predicate
	canviz_graph(:, +, ?, ?).

canviz_graph(Closure, Options) -->
	{ variant_sha1(Closure+Options, Hash),
	  http_session_assert(canviz(Hash, Closure+Options)),
	  http_link_to_id(send_graph, [hash(Hash)], HREF)
	},
	html_requires(js('canviz.js')),
	html([ div(class(graph),
		   div(id(canviz), [])),
	       div(id(debug_output), []),
	       script(type('text/javascript'),
		      \[ 'document.observe(\'dom:loaded\', function() {\n',
			 '  new Canviz(\'canviz\', \'~w\');\n'-[HREF],
			 '});'
		       ])
	     ]).


%%	send_graph(+Request)
%
%	HTTP handler to send a graph.

send_graph(Request) :-
	http_parameters(Request,
			[ hash(Hash, [])
			]),
	http_session_data(canviz(Hash, Closure+Options)),
	call(Closure, Graph),
	length(Graph, Len),
	debug(canviz, 'Graph contains ~D triples', [Len]),
	Renderer = dot,
	process_create(path(Renderer), ['-Txdot'],
		       [ stdin(pipe(ToDOT)),
			 stdout(pipe(XDotOut)),
			 process(PID)
		       ]),
	set_stream(ToDOT, encoding(utf8)),
	set_stream(XDotOut, encoding(utf8)),
	thread_create(send_to_dot(Graph, Options, ToDOT), _,
		      [ detached(true) ]),
	format('Content-type: text/plain; charset=UTF-8\n'),
	format('Transfer-Encoding: chunked\n\n'),
	call_cleanup(copy_graph_data(XDotOut),
		     (	 process_wait(PID, Status),
			 character_count(XDotOut, Count),
			 close(XDotOut),
			 debug(canviz, '~w: ~D bytes, exit status ~w',
			       [Renderer, Count, Status])
		     )).

send_to_dot(Graph, Options, Out) :-
	gviz_write_rdf(Out, Graph, Options),
	close(Out, [force(true)]).

copy_graph_data(Out) :-
	debugging(canviz), !,
	get_code(Out, C0),
	copy_graph_data(C0, Out).
copy_graph_data(Out) :-
	copy_stream_data(Out, current_output).

copy_graph_data(-1, _) :- !.
copy_graph_data(C, Stream) :-
	put_code(C),
	put_code(user_error, C),
	get_code(Stream, C2),
	copy_graph_data(C2, Stream).

