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

:- module(cp_graphviz,
	  [ graphviz_graph//2		% :Closure, +Options
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(process)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(library(semweb/rdf_graphviz)).

:- setting(graphviz:format, oneof([svg,canviz]), svg,
	   'Technique to include RDF graphs in a page').

/** <module> Render RDF-graphs

This module provides graphviz_graph//2 to render   a  list of rdf(S,P,O)
terms as a graph.

@see	library(semweb/rdf_abstract) for various operations on graphs
	represented as lists of rdf(S,P,O).
@tbd	Must be renamed to graphviz.  The current implementation
	supports both canviz and graphviz+SVG.
*/

:- html_resource(js('canviz.js'),
		 [ requires([ js('path/path.js'),
			      js('prototype/prototype.js')
			    ])
		 ]).
:- html_resource(js('path/path.js'),
		 [ requires([ js('prototype/prototype.js')
			    ])
		 ]).

:- http_handler(root(graphviz_send_graph), send_graph, []).

%%	graphviz_graph(:Closure, +Options)//
%
%	Display an RDF graph graphical in the   browser.  The graph is a
%	list  of  rdf(S,P,O)  triples  and    is   obtained  by  calling
%	call(Closure, Graph). This component  inserts   HTML  that  will
%	cause  a  subsequent  call  to    send_graph/1,  which  executes
%	call(Closure,  Graph)  and  sends  the  graph.  This  design  is
%	required for the HTML5/canviz rendering. For   SVG we could have
%	opted for embedded SVG,  but  this   design  is  currently  more
%	portable  and  avoid  slowing  down  page  rendering  if  it  is
%	expensive to produce the graph.
%
%	Options is an option-list for  gviz_write_rdf/3. In addition, it
%	processes the option:
%
%	    * render(+Exe)
%	    Set the rendering engine.  Default is =dot=.
%	    * format(+Format)
%	    One of =canviz=, using AJAX-based rendering on HTML5 canvas
%	    or =svg=, using SVG.  The default is defined by the setting
%	    graphviz:format.
%	    * object_attributes(+List)
%	    Additional attributes to pass to the SVG =object= element.
%
%	This facility requires the graphiz   renderer programs installed
%	in the executable search-path.
%
%	@see http://code.google.com/p/canviz/
%	@see http://www.graphviz.org/

:- meta_predicate
	graphviz_graph(1, :, ?, ?).

graphviz_graph(_Closure, Options) -->
	{ option(render(Renderer), Options, dot)
	},
	\+ { process:exe_options(ExeOptions),
	     absolute_file_name(path(Renderer), _,
				[ file_errors(fail)
				| ExeOptions
				])
	   }, !,
	no_graph_viz(Renderer).
graphviz_graph(Closure, Options) -->
	{ setting(graphviz:format, DefFormat),
	  option(format(Format), Options, DefFormat),
	  meta_options(is_meta, Options, QOptions),
	  variant_sha1(Closure+QOptions, Hash),
	  http_session_assert(graphviz(Hash, Closure+QOptions))
	},
	graphviz_graph_fmt(Format, Hash, QOptions).


graphviz_graph_fmt(canviz, Hash, _Options) --> !,
	{ http_link_to_id(send_graph, [hash(Hash)], HREF)
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
graphviz_graph_fmt(svg, Hash, Options) -->
	{ option(object_attributes(Attrs), Options, []),
	  http_link_to_id(send_graph,
			  [ hash(Hash),
			    lang(svg),
			    target('_top')
			  ], HREF)
	},
	html([ object([ data(HREF),
			type('image/svg+xml')
		      | Attrs
		      ],
		      [])
	     ]).

is_meta(wrap_url).
is_meta(shape_hook).

no_graph_viz(Renderer) -->
	html(div(id('no-graph-viz'),
		 [ 'The server does not have the graphviz program ',
		   code(Renderer), ' installed in PATH. ',
		   'See ', a(href('http://www.graphviz.org/'),
			     'http://www.graphviz.org/'), ' for details.'
		 ])).

%%	send_graph(+Request)
%
%	HTTP handler to send a graph.  This   HTTP  handler is a private
%	handler for graphviz_graph//2, rendering a   list  of rdf(S,P,O)
%	triples using Graphviz.

send_graph(Request) :-
	http_parameters(Request,
			[ hash(Hash,
			       [ description('Hash-key to the graph-data')
			       ]),
			  lang(Lang,
			       [ default(xdot),
				 description('-TXXX option of graphviz')
			       ]),
			  target(Target,
				 [ optional(true),
				   description('Add TARGET= to all links')
				 ])
			]),
	http_session_data(graphviz(Hash, Closure+Options)),
	call(Closure, Graph),
	length(Graph, Len),
	debug(graphviz, 'Graph contains ~D triples', [Len]),
	select_option(render(Renderer), Options, GraphOptions0, dot),
	target_option(Target, GraphOptions0, GraphOptions),
	atom_concat('-T', Lang, GraphLang),
	process_create(path(Renderer), [GraphLang],
		       [ stdin(pipe(ToDOT)),
			 stdout(pipe(XDotOut)),
			 process(PID)
		       ]),
	set_stream(ToDOT, encoding(utf8)),
	set_stream(XDotOut, encoding(utf8)),
	thread_create(send_to_dot(Graph, GraphOptions, ToDOT), _,
		      [ detached(true) ]),
	graph_mime_type(Lang, ContentType),
	format('Content-type: ~w\n', [ContentType]),
	format('Transfer-Encoding: chunked\n\n'),
	call_cleanup(copy_graph_data(XDotOut),
		     (	 process_wait(PID, Status),
			 character_count(XDotOut, Count),
			 close(XDotOut),
			 debug(graphviz, '~w: ~D bytes, exit status ~w',
			       [Renderer, Count, Status])
		     )).

target_option(Target, GraphOptions0, GraphOptions) :-
	(   nonvar(Target)
	->  GraphOptions = [target(Target)|GraphOptions0]
	;   GraphOptions = GraphOptions0
	).


graph_mime_type(xdot, 'text/plain; charset=UTF-8') :- !.
graph_mime_type(svg,  'image/svg+xml') :- !.
graph_mime_type(Lang, 'text/plain; charset=UTF-8') :-
	print_message(warning,
		      format('Do not know content-type for grapviz \
		             language ~w.  Please extend graph_mime_type/2',
			     Lang)).

send_to_dot(Graph, Options, Out) :-
	gviz_write_rdf(Out, Graph, Options),
	close(Out, [force(true)]).

copy_graph_data(Out) :-
	debugging(graphviz), !,
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

