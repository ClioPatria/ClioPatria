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
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_dispatch)).
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

:- create_prolog_flag(html_messages, false, [type(boolean)]).
:- initialization
	asserta((user:message_hook(_Term, Level, Lines) :-
			send_message(Level, Lines))).

call_showing_messages(Goal, Options) :-
	option(style(Style), Options, cliopatria(default)),
	option(head(Head), Options, title('ClioPatria')),
	option(header(Header), Options,
	       div(class(msg_header),
		   h4('Messages ...'))),
	(   option(footer(Footer), Options)
	->  true
	;   (   option(return_to(ReturnURI), Options)
	    ->  FooterRest = [ p(['Go ', a(href(ReturnURI), 'back'),
				  ' to the previous page']) ]
	    ;	FooterRest = []
	    ),
	    Footer = div(class(msg_footer), [ h4('Done') | FooterRest ])
	),
	format('Content-Type: text/html~n'),
	format('Transfer-Encoding: chunked~n~n'),
	header(Style, Head, Header, Footer, FooterTokens),
	setup_call_cleanup(
	    set_prolog_flag(html_messages, true),
	    catch(Goal, E, print_message(error, E)),
	    set_prolog_flag(html_messages, false)), !,
	footer(FooterTokens).

send_message(Level, Lines) :-
	current_prolog_flag(html_messages, true),
	level_css_class(Level, Class),
	phrase(html(pre(class(Class), \html_message_lines(Lines))), Tokens),
	with_mutex(html_messages, print_html(Tokens)),
	flush_output,
	fail.

level_css_class(informational, msg_informational).
level_css_class(warning,       msg_warning).
level_css_class(error,	       msg_error).

html_message_lines([]) -->
	[].
html_message_lines([nl|T]) --> !,
	html('\n'),			% we are in a <pre> environment
	html_message_lines(T).
html_message_lines([flush]) -->
	[].
html_message_lines([H|T]) --> !,
	html(H),
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
