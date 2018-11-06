/*  Part of ClioPatria

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2018, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(yasgui,
          [ has_yasgui/0
          ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/html_head)).

:- use_module(api(json)).               % get /json/prefixes

:- http_handler(yasgui('index.html'), yasgui_editor, [id(yasgui)]).
:- http_handler(yasqe(.), serve_files_in_directory(yasqe), [prefix]).
:- http_handler(yasr(.), serve_files_in_directory(yasr), [prefix]).

%!  yasgui_editor(+Request)
%
%   HTTP handler that presents the YASGUI SPARQL editor.

yasgui_editor(_Request) :-
    has_yasgui,
    !,
    reply_html_page(
        cliopatria(plain),
        title('YASGUI SPARQL Editor'),
        \yasgui_page).
yasgui_editor(_Request) :-
    reply_html_page(cliopatria(default),
                    title('No YASQE/YASR installed'),
                    \no_yasgui).


%!  has_yasgui is semidet.
%
%   True if the YASGUI SPARQL editor is installed.

has_yasgui :-
    Options = [ access(read), file_errors(fail) ],
    absolute_file_name(yasqe('yasqe.bundled.min.js'), _, Options),
    absolute_file_name(yasr('yasr.bundled.min.js'), _, Options).


yasgui_page -->
    { http_link_to_id(sparql_query, [], SparqlLocation),
      http_link_to_id(json_prefixes, [], JSONPrefixes),
      http_link_to_id(list_resource, [], ListResource)
    },
    html_requires(yasqe('yasqe.bundled.min.js')),
    html_requires(yasqe('yasqe.min.css')),
    html_requires(yasr('yasr.bundled.min.js')),
    html_requires(yasr('yasr.min.css')),
    html([ div(id(yasqe), []),
           div(id(yasr), [])
         ]),

    js_script({|javascript(SparqlLocation, JSONPrefixes, ListResource)||
                   window.onload=function(){
  var yasqe = YASQE(document.getElementById("yasqe"), {
                                 sparql: { endpoint: SparqlLocation,
                                           showQueryButton: true
                                         }
                             });

  var serverPrefixes;                   // TBD: re-fetch if out-of-date?

  function usedPrefixes() {
    var prefixmap = yasqe.getPrefixesFromQuery();
    if ( serverPrefixes ) {
      for(var key in serverPrefixes) {
        var yasrKey = key+":";
        if ( !prefixmap[yasrKey] )
          prefixmap[yasrKey] = serverPrefixes[key];
      }
    }
    return prefixmap;
  }

  YASR.plugins.table.defaults.callbacks.onCellClick = function(td, event) {
    var href = YASR.$(td).find("a").attr("href");

    if (href) {
      window.location = ListResource + "?r=" + encodeURIComponent(href);
      event.preventDefault();
    }
  };

  var yasr = {};

  YASQE.$.ajax({ url: JSONPrefixes,
           dataType: "json",
           contentType: 'application/json',
           success: function(data, status) {
                        serverPrefixes = data;
                    },
           complete: function() {
                        yasr = YASR(document.getElementById("yasr"), {
                          getUsedPrefixes: usedPrefixes
                        });
                     }
         });

  /**
  * Set some of the hooks to link YASR and YASQE
  */

  yasqe.options.sparql.callbacks.success = function(data, textStatus, xhr) {
    yasr.setResponse({response: data, contentType: xhr.getResponseHeader("Content-Type")});
  };

  yasqe.options.sparql.callbacks.error = function(xhr, textStatus, errorThrown) {
    var exceptionMsg = textStatus + " (response status code " + xhr.status + ")";
    if (errorThrown && errorThrown.length)
      exceptionMsg += ": " + errorThrown;
    yasr.setResponse({exception: exceptionMsg});
  };
};
              |}).


%!  no_yasgui//
%
%   Display a message indicating the user how to install YASQE/YASR

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
