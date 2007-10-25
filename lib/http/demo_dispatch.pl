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
    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
*/

:- use_module(library('http/thread_httpd')).
:- use_module(library('http/html_write')).
:- use_module(http_dispatch).
:- use_module(http_authenticate).

server(Port) :-
	http_server(http_dispatch,
		    [port(Port)
		    ]).

:- http_handler(/, root, []).
:- http_handler('/request', request, []).
:- http_handler('/fail', do_fail, []).
:- http_handler('/timeout', run, [time_limit(1)]).
:- http_handler(prefix('/protected/'), request,
		[authentication(basic(passwd, 'ByPasswd'))]).
:- http_handler('/protected/request', hello, []).
:- http_handler(prefix('/demo/'), redirect_demo, []).

root(_Request) :-
	reply_page('Root',
		   [ h1('This is the root'),
		     ul([ li(a(href(request), 'Echo request')),
			  li(a(href(not_implemented), 'Test non-existing location')),
			  li(a(href(fail), 'Test failure')),
			  li(a(href(timeout), 'Test time limit (1 sec)')),
			  li(a(href(protected), 'Test authorization prefix')),
			  li(a(href('protected/hello'), 'Test authorization'))
			])
		   ]).

hello(_) :-
	reply_page('Hello', p('Hello world')).

request(Request) :-
	reply_page('Request',
		   [ table([ border(1),
			     cellpadding(2),
			     align(center)
			   ],
			   [ caption('HTTP Request parameters'),
			     tr([th(align(left), 'Property'), th(align(left), 'Value')])
			   | \request_fields(Request)
			   ])
		   ]).

request_fields([]) -->
	[].
request_fields([H|T]) -->
	{ H =.. [Name, Value]
	},
	html(tr([th(align(left), Name), td(\value(Value))])),
	request_fields(T).

value(Value) -->
	{ atomic(Value)
	}, !,
	html(Value).
value(Value) -->
	{ format(string(Q), '~q', [Value])
	}, !,
	html(Q).

%%	do_fail(+Request)
%
%	Force failure to test error reporting.

do_fail(_Request) :-
	fail.

run(_Request) :-
	repeat, fail.

%%	redirect_demo(+Request)
%
%	Redirect /demo/... to ...

redirect_demo(Request) :-
	select(path(Path0), Request, Request1),
	atom_concat('/demo/', Path1, Path0),
	atom_concat('/', Path1, Path),
	http_dispatch([path(Path)|Request1]).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

reply_page(Title, Content) :-
	phrase(page(title(Title), Content), HTML),
	format('Content-type: text/html~n~n'),
	print_html(HTML).

