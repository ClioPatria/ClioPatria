/*  This file is part of ClioPatria.

    Author:
    HTTP:	http://e-culture.multimedian.nl/
    GITWEB:	http://gollem.science.uva.nl/git/ClioPatria.git
    GIT:	git://gollem.science.uva.nl/home/git/ClioPatria.git
    GIT:	http://gollem.science.uva.nl/home/git/ClioPatria.git
    Copyright:  2007, E-Culture/MultimediaN

    ClioPatria is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    ClioPatria is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ClioPatria.  If not, see http://www.gnu.org/licenses/>.
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
