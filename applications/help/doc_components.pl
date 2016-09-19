/*  Part of ClioPatria

    Author:        Michiel Hildebrand
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2012 University of Amsterdam
		             CWI, Asterdam
		             VU University Amsterdam
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

:- module(doc_components,
	  [ api_tester//1,		% +Path
	    api_tester//2,		% Path, Parameters
	    init_api_tester//0
	  ]).

/** <module> Documentation page utilities

@author	Michiel Hildebrand
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(settings)).

/***************************************************
* required html resources
***************************************************/

:- html_resource(js('api_test.js'),
		 [ requires([ js('parameters.js'),
			      yui('button/button.js'),
			      yui('container/container.js'),
			      yui('dragdrop/dragdrop.js'),
			      yui('container/assets/skins/sam/container.css')
			    ])
		 ]).


/***************************************************
* api tester
***************************************************/

%%	api_tester(+Path) is det.
%%	api_tester(+Path, +Parameters) is det.
%
%	Write HTML component with input field and interaction to test an
%	API request.

api_tester(Path) -->
	api_tester(Path, ?).

api_tester(Path, []) --> !,
	html_requires(js('api_test.js')),
	init_api_tester,
	html([ div(class('api_test'),
		   [ table(tbody(tr([ td([ span(class(path), [a(href(Path), Path)])]),
				      td([ input([type(submit), id(submit),
						  value(test),
						  onClick('apiTest(false,"'+Path+'");')])
					 ])
				    ])))
		   ])
	     ]).
api_tester(Path, _) -->
	html_requires(js('api_test.js')),
	init_api_tester,
	html([ div([class('api_test'), width('100%')],
		   [ table(tbody(tr([ td([span(class(path), [Path]), '?']),
				      td(class(input), input([id(api_input), type(text)])),
				      td([ input([type(submit), id(submit), value(test),
						  onClick('apiTest("api_input","'+Path+'");')])
					 ])
				    ]))),
		     p(['Enter key value pairs and separate each by a ', strong('&'),'. ',
		        'In this test field encoding of the values is not needed, \c
			as we do it for you. But, don\'t forget to encode in you \c
			own application.'])
		   ])
	     ]).


init_api_tester -->
	html(script(type('text/javascript'),
		    'initApiPanel();\n')).
