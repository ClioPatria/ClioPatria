/*  Part of ClioPatria

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013-2014 VU University Amsterdam

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
:- use_module(library(http/http_server_files)).
:- use_module(library(http/html_head)).

:- http_handler(yasgui('index.html'), yasgui_editor, []).
:- http_handler(yasqe(.), serve_files_in_directory(yasqe), [prefix]).
:- http_handler(yasr(.), serve_files_in_directory(yasr), [prefix]).

%%	yasgui_editor(+Request)
%
%	HTTP handler that presents the YASGUI SPARQL editor.

yasgui_editor(_Request) :-
	(   \+ absolute_file_name(yasqe('yasqe.min.js'), _,
				  [ access(read),
				    file_errors(fail)
				  ])
	;   \+ absolute_file_name(yasr('yasr.min.js'), _,
				  [ access(read),
				    file_errors(fail)
				  ])
	), !,
	reply_html_page(cliopatria(default),
			title('No YASQE/YASR installed'),
			\no_yasgui).
yasgui_editor(_Request) :-
	reply_html_page(
	    cliopatria(plain),
	    title('YASGUI SPARQL Editor'),
	    \yasgui_page).

yasgui_page -->
	{ http_link_to_id(sparql_query, [], SparqlLocation)
	},
	html_requires(yasqe('yasqe.min.js')),
	html_requires(yasqe('yasqe.min.css')),
	html_requires(yasr('yasr.min.js')),
	html_requires(yasr('yasr.min.css')),
	html([ div(id(yasqe), []),
	       div(id(yasr), [])
	     ]),

	js_script({|javascript(SparqlLocation)||
		   window.onload=function(){
  var yasqe = YASQE(document.getElementById("yasqe"), {
				 sparql: { endpoint: SparqlLocation,
					   showQueryButton: true
					 }
			     });
  var yasr = YASR(document.getElementById("yasr"), {
  // this way, the URLs in the results are prettified using
  // the defined prefixes in the query
			       getUsedPrefixes: yasqe.getPrefixesFromQuery
			   });

  /**
  * Set some of the hooks to link YASR and YASQE
  */

  yasqe.options.sparql.handlers.success = function(data, textStatus, xhr) {
    yasr.setResponse({response: data, contentType: xhr.getResponseHeader("Content-Type")});
  };

  yasqe.options.sparql.handlers.error = function(xhr, textStatus, errorThrown) {
    var exceptionMsg = textStatus + " (response status code " + xhr.status + ")";
    if (errorThrown && errorThrown.length)
      exceptionMsg += ": " + errorThrown;
    yasr.setResponse({exception: exceptionMsg});
  };
};
		   |}).


%%	no_yasgui//
%
%	Display a message indicating the user how to install YASQE/YASR

no_yasgui -->
	{ absolute_file_name(cliopatria(.), CD0,
			     [ file_type(directory),
			       access(read)
			     ]),
	  prolog_to_os_filename(CD0, ClioHome)
	},
	html_requires(pldoc),
	html([ h1('YASGUI (YASQE and YASR) is not installed'),
	       p([ 'Please run the following command in the ClioPatria ',
		   'installation directory "~w" to install YASQE/YASR.'-[ClioHome]
		 ]),
	       pre(class(code),
		   [ 'git submodule update --init web/yasqe web/yasr'
		   ])
	     ]).
