/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, VU University Amsterdam

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

:- module(cp_messages,
	  [ call_showing_messages/2
	  ]).
:- use_module(library(http/html_write)).
:- use_module(library(option)).

/** <module> Run goals that produce messages

This module allows executing (long  running)   Prolog  goals and see the
messages appear in the browser.
*/

:- meta_predicate
	call_showing_messages(0, +).

call_showing_messages(Goal, Options) :-
	option(style(Style), Options, cliopatria(default)),
	option(head(Head), Options, title('ClioPatria')),
	format('Content-Type: text/html~n'),
	format('Transfer-Encoding: chunked~n~n'),
	header(Style, Head),
	thread_self(Me),
	setup_call_cleanup(asserta((user:message_hook(_Term, Level, Lines) :-
				   	send_message(Me, Level, Lines)), Ref),
			   Goal,
			   erase(Ref)), !,
	footer.

send_message(Me, Level, Lines) :-
	thread_self(Me),
	phrase(html(div(class(Level), \html_message_lines(Lines))), Tokens),
	print_html(Tokens),
	flush_output,
	fail.

html_message_lines([]) -->
	[].
html_message_lines([nl|T]) --> !,
	html([br([])]),
	html_message_lines(T).
html_message_lines([flush]) -->
	[].
html_message_lines([Fmt-Args|T]) --> !,
	{ format(string(S), Fmt, Args)
	},
	html([S]),
	html_message_lines(T).
html_message_lines([Fmt|T]) --> !,
	{ format(string(S), Fmt, [])
	},
	html([S]),
	html_message_lines(T).


%%	header(+Style, +Head)
%
%

header(Style, Head) :-
	phrase(html_write:(doctype,
			   html_begin(html),
			   pagehead(Style, Head)), Tokens),
	print_html(Tokens),
	flush_output.

footer :-
	phrase(html_write:html_end(html), Tokens),
	print_html(Tokens).
