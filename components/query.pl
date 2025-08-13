/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam,
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

:- module(rdfql_queries,
          [ query_form//1,              % +Options
            store_recall//2,            % +Type, +ColsStore-CollsRecall
            query_script//0,            %
            store_query/3               % +Type, +Id, +Query
          ]).
:- use_module(library(http/http_session)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(basics).
:- use_module(library(option)).
:- use_module(library(settings)).

/** <module> Forms for entering SPARQL and SeRQL queries.

This module implements the forms for   entering SPARQL and SeRQL queries
with a simple query-history mechanism for user-submitted SPARQL queries.
*/

%!  query_form(+Options)//
%
%   HTMP component for an  interactive   (SPARQL)  query-form.  This
%   calls to the handler with  id   =evaluate_query=.  Options is an
%   option list:
%
%       * query_languages(+List)
%       Query languages supported.  Default is ['SPARQL', 'SeRQL'].
%       Specifying only one removes the query-language menu.

query_form(Options) -->
    html([ form([ class(query),
                  name(query),
                  action(location_by_id(evaluate_query)),
                  method('GET')
                ],
                [ \hidden(repository, default),
                  \hidden(serialization, rdfxml),
                  h3([ 'Interactive ',
                       \query_language(Options, Hidden),
                       ' query'
                     ]),
                  Hidden,
                  table([ class(query)
                        ],
                        [ \store_recall(_, 3-2),
                          tr([ td(colspan(5),
                                  textarea(name(query), ''))
                             ]),
                          tr([ td([ span(class(label), 'Result format: '),
                                    \result_format
                                  ]),
                               td([ span(class(label), 'Resource: '),
                                    \resource_menu
                                  ]),
                               td([ span(class(label), 'Entailment: '),
                                    \entailment
                                  ]),
                               td(align(right),
                                  [ input([ type(reset),
                                            value('Clear')
                                          ]),
                                    input([ type(submit),
                                            value('Go!')
                                          ])
                                  ])
                             ])
                        ])
                ]),
           \query_script
         ]).


result_format -->
    html(select(name(resultFormat),
                [ option([], xml),
                  option([selected], html),
                  option([], json),
                  option([], csv)
                ])).

query_language(Options, Hidden) -->
    { option(query_languages(LangList), Options, ['SPARQL', 'SeRQL'])
    },
    (   { LangList = [Lang] }
    ->  html([Lang]),
        { Hidden = \hidden(queryLanguage, Lang) }
    ;   { LangList = [DefLang|More] },
        html(select(name(queryLanguage),
                    [ option([selected], DefLang)
                    | \options(More)
                    ])),
        { Hidden = '' }
    ).

options([]) --> [].
options([Value|T]) -->
    html(option([], Value)),
    options(T).


resource_menu -->
    html(select(name(resourceFormat),
                [ option([value(plain)],            plain),
                  option([value(ns), selected],     'ns:local'),
                  option([value(nslabel)],          'ns:label')
                ])).

entailment -->
    { findall(E, cliopatria:entailment(E, _), Es)
    },
    html(select(name(entailment),
                \entailments(Es))).

entailments([]) -->
    [].
entailments([E|T]) -->
    (   { setting(cliopatria:default_entailment, E)
        }
    ->  html(option([selected], E))
    ;   html(option([], E))
    ),
    entailments(T).


%!  store_recall(+Type, +ColsSpec)// is det.
%
%   Creates a table-row  (=tr=)  holding   a  `store'  and  `recall'
%   element. ColsSpec is a term   SpanLeft-SpanRight, containing the
%   colspan-attribute for both created table-cells. Note that a page
%   including this must also  include   query_script//0  at  a place
%   later in the page where a script is allowed.

store_recall(Type, SL-SR) -->
    { next_query_id(Id), !
    },
    html(tr([ td([ class(qstore),
                   colspan(SL)
                 ],
                 [ b('Remember as: '),
                   input([ id(qid),
                           name(storeAs),
                           size(30),
                           value(Id)
                         ])
                 ]),
              td([ class(qrecall),
                   colspan(SR),
                   align(right)
                 ],
                 \recall(Type))
            ])).
store_recall(_, SL-SR) -->
    { Span is SL+SR },
    html(tr([ td([ class(qnostore),
                   colspan(Span)
                 ],
                 [ 'Login to enable save/restore of queries'
                 ])
            ])).


recall(Type) -->
    { http_in_session(_),
      findall(Name-Query, stored_query(Name, Type, Query), Pairs),
      Pairs \== []
    },
    !,
    html([ b('Recall: '),
           select(name(recall),
                  [ option([selected], '')
                  | \stored_queries(Pairs)
                  ])
         ]).
recall(_) -->
    [].

:- thread_local
    script_fragment/1.

stored_queries([]) --> !.
stored_queries(List) -->
    stored_queries(List, 1),
    { assert(script_fragment('\nf1();\n')) }.

stored_queries([], _) -->
    [].
stored_queries([Name-Query|T], I) -->
    { I2 is I + 1,
      atom_concat(f, I, FName),
      js_quoted(Query, QuotedQuery),
      format(atom(Script),
             'function ~w()\n\c
                 { document.query.query.value=\'~w\';\n  \c
                   document.getElementById(\'qid\').value="~w";\n\c
                 }\n',
             [ FName, QuotedQuery, Name ]),
      assert(script_fragment(Script)),
      format(atom(Call), '~w()', [FName])
    },
    html(option([onClick(Call)], Name)),
    stored_queries(T, I2).

%!  query_script//
%
%   Inserts the <script> holding JavaScript functions that restore
%   the queries.
%
%   @tbd    This must be rewritten to use the post/receive mechanism.

query_script -->
    { findall(S, retract(script_fragment(S)), Fragments),
      Fragments \== []
    },
    !,
    [ '\n<script language="JavaScript">\n'
    ],
    Fragments,
    [ '\n</script>\n'
    ].
query_script -->
    [].

%!  js_quoted(+Raw, -Quoted)
%
%   Quote text for use in JavaScript. Quoted does _not_ include the
%   leading and trailing quotes.

js_quoted(Raw, Quoted) :-
    atom_codes(Raw, Codes),
    phrase(js_quote_codes(Codes), QuotedCodes),
    atom_codes(Quoted, QuotedCodes).

js_quote_codes([]) -->
    [].
js_quote_codes([0'\r,0'\n|T]) -->
    !,
    "\\n",
    js_quote_codes(T).
js_quote_codes([H|T]) -->
    js_quote_code(H),
    js_quote_codes(T).

js_quote_code(0'') -->
    !,
    "\\'".
js_quote_code(0'\\) -->
    !,
    "\\\\".
js_quote_code(0'\n) -->
    !,
    "\\n".
js_quote_code(0'\r) -->
    !,
    "\\r".
js_quote_code(0'\t) -->
    !,
    "\\t".
js_quote_code(C) -->
    [C].


                 /*******************************
                 *         SAVED QUERIES        *
                 *******************************/

%!  store_query(+Type, +Name, +Query) is det.
%
%   Store the SPARQL/SeRQL Query under Name  in the current session.
%   Succeeds without doing anything if there is no session.

store_query(_, '', _) :- !.
store_query(Type, As, Query) :-
    http_in_session(_),
    !,
    set_high_id(As),
    http_session_retractall(stored_query(As, Type, _)),
    http_session_retractall(stored_query(_, Type, Query)),
    http_session_asserta(stored_query(As, Type, Query)).
store_query(_, _, _).

stored_query(As, Type, Query) :-
    http_session_data(stored_query(As, Type, Query)).

set_high_id(Name) :-
    http_in_session(_),
    atom_concat('Q-', Id, Name),
    catch(atom_number(Id, N), _, fail),
    !,
    (   http_session_data(qid(N0))
    ->  (   N > N0
        ->  http_session_retract(qid(_)),
            http_session_assert(qid(N))
        ;   true
        )
    ;   http_session_assert(qid(N))
    ).
set_high_id(_).


next_query_id(Id) :-
    http_in_session(_Session),
    !,
    (   http_session_data(qid(Id0))
    ->  Next is Id0+1
    ;   Next is 1
    ),
    atomic_list_concat(['Q-',Next], Id).
