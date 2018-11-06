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

:- module(cp_label,
          [ turtle_label//1,            % +Literal
            rdf_link//1,                % +RDFTerm
            rdf_link//2,                % +RDFTerm, +Options
            resource_link/2             % +URI, -URL
          ]).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(library(aggregate)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- if(exists_source(library(semweb/rdf11))).
:- use_module(library(semweb/rdf11), [rdf_lexical_form/2]).
:- endif.

:- use_module(cliopatria(hooks)).

/** <module> Support for showing labels

This module provides HTML components to display labels for resources.

@see    library(semweb/rdf_label) returns textual labels.
*/


%!  turtle_label(+RDFTerm)// is det.
%
%   HTML  rule  to  emit  an  RDF   term  (resource  or  object)  in
%   turtle-like notation with CSS classes.
%
%   @tbd    Implement possibility for a summary.

turtle_label(R) -->
    turtle_label(R, []).

turtle_label(R, _) -->
    { atom(R),
      rdf_global_id(NS:Local, R), !
    },
    html([span(class(prefix), NS), ':', span(class(local), Local)]).
turtle_label(R, Options) -->
    { atom(R),
      rdf_display_label(R, Lang, LabelText),
      Lang \== url,
      LabelText \== '',
      truncate_text(LabelText, Show, Options)
    },
    html(Show).
turtle_label(R, Options) -->
    { rdf_is_bnode(R) },
    bnode_label(R, Options),
    !.
turtle_label(R, _) -->
    { atom(R) },
    !,
    html(['<',R,'>']).
turtle_label(literal(Lit), Options) -->
    !,
    literal_label(Lit, Options).
turtle_label(@(String,Lang), Options) -->
    !,
    literal_label(lang(Lang, String), Options).
:- if(current_predicate(rdf_lexical_form/2)).
turtle_label(^^(Value,Type), Options) -->
    !,
    (   {rdf_equal(Type, xsd:string)}
    ->  literal_label(type(Type, Value), Options)
    ;   {rdf_lexical_form(^^(Value,Type), ^^(String,_))},
        literal_label(type(Type, String), Options)
    ).
:- endif.

literal_label(type(Type, Value), Options) -->
    !,
    { truncate_text(Value, Show, Options) },
    html(span(class(literal),
              [span(class(oquote), '"'), span(class(l_text), Show), span(class(cquote), '"'),
               span(class(l_type), '^^'), \turtle_label(Type)])).
literal_label(lang(Lang, Value), Options) -->
    !,
    { truncate_text(Value, Show, Options) },
    html(span(class(literal),
              [span(class(oquote), '"'), span(class(l_text), Show), span(class(cquote), '"'),
               span(class(l_lang), '@'), span(class(lang), Lang)])).
literal_label(Value, Options) -->
    { truncate_text(Value, Show, Options) },
    html(span(class(literal),
              [span(class(oquote), '"'), span(class(l_text), Show), span(class(cquote), '"')])).

truncate_text(Text, Text, []) :- !.
truncate_text(Text, Truncated, Options) :-
    option(max_length(Len), Options),
    !,
    truncate_atom(Text, Len, Truncated).
truncate_text(Text, Text, _).


%!  bnode_label(+Resource, +Options)// is semidet.
%
%   Display an HTML label for an  RDF   blank  node.  This DCG rules
%   first  calls  the  hook  cliopatria:bnode_label//1.  On  failure
%   performs its default task:
%
%       * If the bnode has an rdf:value, display the label thereof
%       with [<label>...]
%
%       * If the bnode is an RDF collection, display its first 5
%       members as (<member-1>, <member-2, ...)

bnode_label(R, _) -->
    cliopatria:bnode_label(R),
    !.
bnode_label(R, Options) -->
    { rdf_has(R, rdf:value, Value),
      (   Value = literal(_)
      ;   \+ rdf_is_bnode(Value)
      )
    },
    !,
    html(span([ class(rdf_bnode),
                title('RDF bnode using rdf:value')
              ],
              ['[', \turtle_label(Value, Options), '...]'])).
bnode_label(R, Options) -->
    { rdf_collection_list(R, List),
      !,
      length(List, Len),
      format(string(Title), 'RDF collection with ~D members', Len)
    },
    html(span([ class(rdf_list),
                title(Title)
              ],
              ['(', \collection_members(List, 0, Len, 5, Options), ')'])).

collection_members([], _, _, _, _) --> [].
collection_members(_, Max, Total, Max, _) -->
    !,
    { Left is Total - Max },
    html('... ~D more'-[Left]).
collection_members([H|T], I, Total, Max, Options) -->
    turtle_label(H, Options),
    (   { T == [] }
    ->  []
    ;   html(','),
        { I2 is I + 1 },
        collection_members(T, I2, Total, Max, Options)
    ).


rdf_collection_list(R, []) :-
    rdf_equal(rdf:nil, R),
    !.
rdf_collection_list(R, [H|T]) :-
    rdf_has(R, rdf:first, H),
    rdf_has(R, rdf:rest, RT),
    rdf_collection_list(RT, T).


%!  rdf_link(+URI)// is det.
%!  rdf_link(+URI, +Options)// is det.
%
%   Make a hyper-link to an arbitrary   RDF resource or object using
%   the label.  Options processed:
%
%       * resource_format(+Format)
%       Determines peference for displaying resources.  Values are:
%
%           * plain
%           Display full resource a plain text
%           * label
%           Try to display a resource using its label
%           * nslabel
%           Try to display a resource as <prefix>:<Label>
%           * turtle
%           Try to display as Turtle <prefix>:<local>
%       * max_length(+Len)
%       Truncate long texts to Len characters, using ellipses to
%       indicate that the text is truncated.
%       * target(+Target)
%       Passed to the HTML <a> element as `target` attribute.
%       * role(+Role)
%       Passed to cliopatria:display_link/2 hook as option.
%       Can be used to differentiate display of URI depending on role
%       as subject, predicate, object, bnode, domain, or range.
%
%   This predicate creates two types of  links. Resources are linked
%   to the handler implementing   =list_resource= using r=<resource>
%   and  literals  that  appear  multiple    times   are  linked  to
%   =list_triples_with_object= using a Prolog  representation of the
%   literal.
%
%   This predicate can be hooked using cliopatria:display_link//2.
%
%   @tbd    Make it easier to determine the format of the label
%   @tbd    Allow linking to different handlers.

rdf_link(R) -->
    rdf_link(R, []).

rdf_link(R, Options) -->
    cliopatria:display_link(R, Options),
    !.
rdf_link(R, Options) -->
    { atom(R),
      !,
      resource_link(R, HREF),
      (   rdf(R, _, _)
      ->  Class = r_def
      ;   rdf_graph(R)
      ->  Class = r_graph
      ;   Class = r_undef
      ),
      link_options(Extra, Options)
    },
    html(a([class(['rdf-r',Class]), href(HREF)|Extra],
           \resource_label(R, Options))).
rdf_link(Literal, Options) -->
    { aggregate_all(count, literal_occurrence(Literal, Options), Count),
      Count > 1,
      !,
      format(string(Title), 'Used ~D times', [Count]),
      term_to_atom(Literal, Atom),
      http_link_to_id(list_triples_with_object, [l=Atom], HREF),
      link_options(Extra, Options)
    },
    html(a([ class(l_count),
             href(HREF),
             title(Title)
           | Extra
           ],
           \turtle_label(Literal))).
rdf_link(Literal, _) -->
    turtle_label(Literal).

literal_occurrence(Literal, Options) :-
    Literal = literal(_),
    !,
    (   option(graph(Graph), Options)
    ->  rdf_db:rdf(_,_,Literal,Graph)
    ;   rdf_db:rdf(_,_,Literal)
    ).
:- if(current_predicate(rdf11:rdf/4)).
literal_occurrence(Literal, Options) :-
    (   option(graph(Graph), Options)
    ->  rdf11:rdf(_,_,Literal,Graph)
    ;   rdf11:rdf(_,_,Literal)
    ).
:- endif.

link_options(LinkOptions, Options) :-
    option(target(Target), Options),
    !,
    LinkOptions = [target(Target)].
link_options([], _).


%!  resource_link(+URI, -URL) is det.
%
%   Generate a link to display more   information  about a resource.
%   The  default  is  to  link  to  the  HTTP  handler  implementing
%   =list_resource=     using     the     parameter     =r=.     See
%   cpa_browse:list_resource/1.  This  predicate  calls    the  hook
%   cliopatria:resource_link/2,  which  allows  for  overruling  the
%   default.

resource_link(R, HREF) :-
    cliopatria:resource_link(R, HREF),
    !.
resource_link(R, HREF) :-
    http_link_to_id(list_resource, [r=R], HREF).

resource_label(R, Options) -->
    { debug(rdf(label), 'resource_label(~p,~p)',
            [R, Options]),
      option(resource_format(Format), Options)
    },
    !,
    resource_flabel(Format, R, Options).
resource_label(R, Options) -->
    turtle_label(R, Options).

resource_flabel(plain, R, _) -->
    !,
    html(R).
resource_flabel(label, R, Options) -->
    !,
    (   { rdf_display_label(R, Label),
          truncate_text(Label, Show, Options)
        }
    ->  html([span(class(r_label), Show)])
    ;   turtle_label(R)
    ).
resource_flabel(nslabel, R, _Options) -->
    { (   rdf_is_bnode(R)
      ->  NS = '_'
      ;   rdf_global_id(NS:_Local, R)
      ->  true
      ;   NS = '?'
      ),
      !,
      rdf_display_label(R, Label)
    },
    html([span(class(prefix),NS),':',span(class(r_label),Label)]).
resource_flabel(_, R, Options) -->
    turtle_label(R, Options).
