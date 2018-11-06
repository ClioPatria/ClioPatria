/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, VU University Amsterdam
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

:- module(cp_messages,
          [ call_showing_messages/2,    % :Goal, +Options
            after_messages/1            % +HTML
          ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/cp_jquery)).
:- use_module(library(option)).
:- use_module(library(lists)).

/** <module> Run goals that produce messages

This module allows executing (long  running)   Prolog  goals and see the
messages appear in the browser.
*/

:- meta_predicate
    call_showing_messages(0, +).
:- html_meta
    after_messages(html).

%!  call_showing_messages(:Goal, +Options) is det.
%
%   Execute  Goal,  showing  the  feedback   in  the  browser.  This
%   predicate builds a default application   page with a placeholder
%   for the messages. It then sends   all  HTML upto the placeholder
%   and flushes the output to  the   browser.  During execution, all
%   output from Goal emitted through   print_message/2  is caught in
%   the message-box. After completion of Goal the page is completed.
%
%   This predicate is intended for action such as loading RDF files,
%   while providing feedback on  files   loaded  and  possible error
%   messages. Note that this call creates a complete page.
%
%   @bug    This call uses =chunked= transfer encoding to send the
%           page in parts.  Not all browsers support this and not
%           all browsers update the page incrementally.

:- create_prolog_flag(html_messages, false, [type(boolean)]).
assert_message_hook :-
    Head = user:message_hook(_Term, Level, Lines),
    Body = send_message(Level, Lines),
    (   clause(Head, Body)
    ->  true
    ;   asserta((Head:-Body))
    ).
:- initialization
    assert_message_hook.


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
        ;   FooterRest = []
        ),
        Footer = div(class(msg_footer), [ h4('Done') | FooterRest ])
    ),
    format('Content-Type: text/html~n'),
    format('Transfer-Encoding: chunked~n~n'),
    header(Style, Head, Header, Footer, FooterTokens),
    setup_call_cleanup(
        set_prolog_flag(html_messages, true),
        catch(once(Goal), E, print_message(error, E)),
        set_prolog_flag(html_messages, false)),
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
level_css_class(error,         msg_error).

html_message_lines([]) -->
    [].
html_message_lines([nl|T]) -->
    !,
    html('\n'),                     % we are in a <pre> environment
    html_message_lines(T).
html_message_lines([flush]) -->
    [].
html_message_lines([H|T]) -->
    !,
    html(H),
    html_message_lines(T).


%!  after_messages(+HTML) is det.
%
%   Close the message window and emit   HTML.  This predicate may be
%   called from the Goal of call_showing_messages/2 to indicate that
%   all work has been done.

after_messages(HTML) :-
    close_messages,
    phrase(html(HTML), Tokens),
    current_output(Out),
    html_write:write_html(Tokens, Out).


%!  header(+Style, +Head, +Header, +Footer, -FooterTokens)
%
%   Emit all tokens upto the placeholder for the actual messages and
%   return the remaining page-tokens in FooterTokens. Style and Head
%   are passed

header(Style, Head, Header, Footer, FooterTokens) :-
    http_absolute_location(icons('smiley-thinking.gif'), Image, []),
    Magic = '$$$MAGIC$$$',
    make_list(Header, HList),
    make_list(Footer, FList),
    append([ HList,
             [ \(cp_messages:html_requires(jquery)),
               img([id('smiley-thinking'), src(Image)]),
               div(class(messages), Magic),
               \(cp_messages:js_script({|javascript||
                                        $("#smiley-thinking").hide(1000)|}))
             ],
             FList
           ], Body),
    phrase(html_write:page(Style, Head, Body), Tokens),
    html_write:mailman(Tokens),
    (   append(HeaderTokens, [Magic|FooterTokens0], Tokens)
    ->  append(CloseDiv0, [>|FooterTokens], FooterTokens0)
    ->  append(CloseDiv0, [>], CloseDiv)
    ->  true
    ),
    nb_setval(html_messages_close, CloseDiv),
    current_output(Out),
    html_write:write_html(HeaderTokens, Out),
    flush_output(Out).

make_list(List, List) :-
    is_list(List),
    !.
make_list(Obj, [Obj]).

close_messages :-
    nb_current(html_messages_close, Tokens),
    !,
    nb_delete(html_messages_close),
    current_output(Out),
    html_write:write_html(Tokens, Out).
close_messages.

footer(FooterTokens) :-
    close_messages,
    current_output(Out),
    html_write:write_html(FooterTokens, Out).
