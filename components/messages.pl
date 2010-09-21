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

%%	call_showing_messages(:Goal, +Options) is det.
%
%	Execute  Goal,  showing  the  feedback   in  the  browser.  This
%	predicate builds a default application   page with a placeholder
%	for the messages. It then sends   all  HTML upto the placeholder
%	and flushes the output to  the   browser.  During execution, all
%	output from Goal emitted through   print_message/2  is caught in
%	the message-box. After completion of Goal the page is completed.
%
%	This predicate is intended for action such as loading RDF files,
%	while providing feedback on  files   loaded  and  possible error
%	messages. Note that this call creates a complete page.
%
%	@bug	This call uses =chunked= transfer encoding to send the
%		page in parts.  Not all browsers support this and not
%		all browsers update the page incrementally.

call_showing_messages(Goal, Options) :-
	option(style(Style), Options, cliopatria(default)),
	option(head(Head), Options, title('ClioPatria')),
	option(header(Header), Options, h4('Messages ...')),
	option(footer(Footer), Options, h4('Done')),
	format('Content-Type: text/html~n'),
	format('Transfer-Encoding: chunked~n~n'),
	header(Style, Head, Header, Footer, FooterTokens),
	thread_self(Me),
	setup_call_cleanup(asserta((user:message_hook(_Term, Level, Lines) :-
				   	send_message(Me, Level, Lines)), Ref),
			   Goal,
			   erase(Ref)), !,
	footer(FooterTokens).

send_message(Me, Level, Lines) :-
	thread_self(Me),
	Level \== silent,
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


%%	header(+Style, +Head, +Header, +Footer, -FooterTokens)
%
%	Emit all tokens upto the placeholder for the actual messages and
%	return the remaining page-tokens in FooterTokens. Style and Head
%	are passed

header(Style, Head, Header, Footer, FooterTokens) :-
	Magic = '$$$MAGIC$$$',
	Body = [ Header,
		 div(class(messages), Magic),
		 Footer
	       ],
	phrase(html_write:page(Style, Head, Body), Tokens),
	html_write:mailman(Tokens),
	append(HeaderTokens, [Magic|FooterTokens], Tokens), !,
	current_output(Out),
	html_write:write_html(HeaderTokens, Out),
	flush_output(Out).

footer(Footer) :-
	current_output(Out),
	html_write:write_html(Footer, Out).
