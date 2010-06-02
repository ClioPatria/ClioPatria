/*  This file is part of ClioPatria.

    Author:	Jan Wielemaker <wielemak@science.uva.nl>
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
    along with ClioPatria.  If not, see <http://www.gnu.org/licenses/>.
*/

:- module(yui_resources,
	  [
	  ]).
:- use_module(library(http/http_hook)).
:- use_module(library(http/html_head)).
:- use_module(library(settings)).

/** <module> Define HTML resources (scripts and CSS files)

This file defines the HTTP locations of   resources for HTML pages, such
as scripts and CSS files, as  well   as  their dependencies. With proper
dependency declarations including a script and   all  dependencies is as
simple as embedding this in the  HTML   generating  DCGs,  either in the
document head or the document body.   The  html_post/2 mechanism ensures
duplicates are removed and the stylesheet links and javascript relations
end up in the HTML =head=.

==
	...
	html_requires(script('fbrowse.js'))
==
*/

		 /*******************************
		 *    SCRIPT & CSS LOCATIONS	*
		 *******************************/

% Define HTTP location paths, similar to the definition of file search
% paths.

http:location(www,	    root(www),		       []).
http:location(script,	    www(script),	       [js(true)]).
http:location(yui,	    yui_base(build),	       [js(true)]).
http:location(yui_examples, yui_base(examples),	       [js(true)]).

:- if(absolute_file_name(yui(.), _, [ access(read),
				      file_type(directory),
				      file_errors(fail)
				    ])).
http:location(yui_base,    www('yui/2.7.0'),	       []).

:- use_module(library(http/http_dispatch)).

:- http_handler(yui_base(.), serve_file, [prefix]).

serve_file(Request) :-
	memberchk(path_info(Path), Request),
	http_reply_file(yui(Path), [], Request).

:- else.

http:location(yui_base,	    'http://yui.yahooapis.com/2.7.0/', []).

:- endif.

		 /*******************************
		 *	YUI DEPENDENCIES	*
		 *******************************/

% NOTE: These may be wrong or incomplete. They have been derived using a
% simple script based on the @require statements in the YUI code.

:- html_resource(yui('animation/animation.js'),
		 [ requires([ '../dom/dom.js',
			      '../event/event.js',
			      '../yahoo/yahoo.js'
			    ])
		 ]).
:- html_resource(yui('autocomplete/autocomplete.js'),
		 [ requires([ '../datasource/datasource.js',
			      '../connection/connection.js',
			      '../dom/dom.js',
			      '../event/event.js',
			      '../yahoo/yahoo.js'
			    ])
		 ]).
:- html_resource(yui('button/button.js'),
		 [ requires([ '../dom/dom.js',
			      '../event/event.js',
			      '../yahoo/yahoo.js',
			      '../element/element-beta.js'
			    ])
		 ]).
:- html_resource(yui('calendar/calendar.js'),
		 [ requires([ '../dom/dom.js',
			      '../event/event.js',
			      '../yahoo/yahoo.js'
			    ])
		 ]).
:- html_resource(yui('colorpicker/colorpicker.js'),
		 [ requires([ '../dom/dom.js',
			      '../event/event.js',
			      '../slider/slider.js',
			      '../yahoo/yahoo.js'
			    ])
		 ]).
:- html_resource(yui('connection/connection.js'),
		 [ requires([ '../event/event.js',
			      '../yahoo/yahoo.js'
			    ])
		 ]).
:- html_resource(yui('container/container.js'),
		 [ requires([ '../dom/dom.js',
			      '../event/event.js',
			      '../yahoo/yahoo.js'
			    ])
		 ]).
:- html_resource(yui('datasource/datasource.js'),
		 [ requires([ '../event/event.js',
			      '../yahoo/yahoo.js'
			    ])
		 ]).
:- html_resource(yui('datatable/datatable.js'),
		 [ requires([ '../dom/dom.js',
			      '../event/event.js',
			      '../yahoo/yahoo.js',
			      '../datasource/datasource.js',
			      '../element/element-beta.js'
			    ])
		 ]).

:- html_resource(yui('dom/dom.js'),
	 [ requires([ '../yahoo/yahoo.js' ])
	 ]).

:- html_resource(yui('event/event.js'),
	 [ requires([ '../yahoo/yahoo.js' ])
	 ]).

:- html_resource(yui('dragdrop/dragdrop.js'),
		 [ requires([ '../dom/dom.js',
			      '../event/event.js',
			      '../yahoo/yahoo.js'
			    ])
		 ]).
:- html_resource(yui('editor/editor.js'),
		 [ requires([ '../element/element-beta.js',
			      '../container/container.js',
			      '../dom/dom.js',
			      '../event/event.js',
			      '../menu/menu.js',
			      '../yahoo/yahoo.js'
			    ])
		 ]).
:- html_resource(yui('element/element-beta.js'),
		 [ requires([ '../dom/dom.js',
			      '../event/event.js',
			      '../yahoo/yahoo.js'
			    ])
		 ]).
:- html_resource(yui('event/event.js'),
		 [ requires([ '../yahoo/yahoo.js'
			    ])
		 ]).
:- html_resource(yui('history/history.js'),
		 [ requires([ '../event/event.js',
			      '../yahoo/yahoo.js'
			    ])
		 ]).
:- html_resource(yui('json/json.js'),
		 [ requires([ '../yahoo/yahoo.js' ])
		 ]).
:- html_resource(yui('logger/logger.js'),
		 [ requires([ '../dom/dom.js',
			      '../event/event.js',
			      '../yahoo/yahoo.js'
			    ])
		 ]).
:- html_resource(yui('menu/menu.js'),
		 [ requires([ '../container/container.js',
			      '../dom/dom.js',
			      '../event/event.js'
			    ])
		 ]).
:- html_resource(yui('slider/slider.js'),
		 [ requires([ '../dom/dom.js',
			      '../dragdrop/dragdrop.js',
			      '../event/event.js',
			      '../yahoo/yahoo.js'
			    ])
		 ]).
:- html_resource(yui('tabview/tabview.js'),
		 [ requires([ '../dom/dom.js',
			      '../event/event.js',
			      '../yahoo/yahoo.js',
			      '../element/element-beta.js'
			    ])
		 ]).
:- html_resource(yui('treeview/treeview.js'),
		 [ requires([ '../event/event.js',
			      '../yahoo/yahoo.js'
			    ])
		 ]).
:- html_resource(yui('carousel/carousel.js'),
		 [ requires([ '../dom/dom.js',
			      '../event/event.js',
			      '../yahoo/yahoo.js',
			      '../element/element-beta.js',
			      '../animation/animation.js',
			      '../connection/connection.js'
			    ])
		 ]).
:- html_resource(yui('charts/charts.js'),
		 [ requires([ '../dom/dom.js',
			      '../event/event.js',
			      '../yahoo/yahoo.js',
			      '../element/element-beta.js',
			      '../datasource/datasource.js'
			    ])
		 ]).
:- html_resource(yui('resize/resize.js'),
		 [ requires([ '../dom/dom.js',
			      '../event/event.js',
			      '../yahoo/yahoo.js',
			      '../element/element-beta.js',
			      '../dragdrop/dragdrop.js',
			      '../animation/animation.js'
			    ])
		 ]).
:- html_resource(yui('paginator/paginator.js'),
		 [ requires([ '../dom/dom.js',
			      '../event/event.js',
			      '../yahoo/yahoo.js',
			      '../element/element-beta.js'
			    ])
		 ]).
:- html_resource(yui('layout/layout.js'),
		 [ requires([ '../dom/dom.js',
			      '../event/event.js',
			      '../yahoo/yahoo.js',
			      '../element/element-beta.js',
			      '../dragdrop/dragdrop.js',
			      '../resize/resize.js',
			      '../animation/animation.js'
			    ])
		 ]).

:- html_resource(yui('yahoo/yahoo.js'), []).
:- html_resource(yui('yuiloader/yuiloader-beta.js'), []).

% YUI aggregates


:- html_resource(yui('utilities/utilities.js'),
		 [ aggregate([ '../yahoo/yahoo.js',
			       '../dom/dom.js',
			       '../event/event.js',
			       '../connection/connection.js',
			       '../animation/animation.js',
			       '../dragdrop/dragdrop.js',
			       '../element/element-beta.js'
			     ])
		 ]).

:- html_resource(yui('yahoo-dom-event/yahoo-dom-event.js'),
		 [ aggregate([ '../yahoo/yahoo.js',
			       '../dom/dom.js',
			       '../event/event.js'
			     ])
		 ]).
:- html_resource(yui('reset-fonts-grids/reset-fonts-grids.css'),
		 [ aggregate([ '../reset/reset.css',
			       '../fonts/fonts.css',
			       '../grids/grids.css'
			     ])
		 ]).
