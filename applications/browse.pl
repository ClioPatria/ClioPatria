/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2025, VU University Amsterdam
                              CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(cpa_browse,
          [ graph_info//1,              % +Graph
            graph_as_resource//2,       % +Graph, +Options
            graph_actions//1,           % +Graph
            list_resource//2,           % +URI, +Options
            context_graph//2,           % +URI, +Options
            instance_table//1           % +Options
          ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/yui_resources)).
:- use_module(library(http/http_path)).
:- use_module(library(http/cp_jquery)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(semweb/rdf_persistency)).

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(settings)).
:- use_module(library(xpath)).

:- use_module(components(label)).
:- use_module(components(simple_search)).
:- use_module(components(graphviz)).
:- use_module(components(basics)).
:- use_module(api(lod_crawler)).
:- use_module(api(sesame)).
:- use_module(library(semweb/rdf_abstract)).
:- use_module(library(semweb/rdf_label)).

:- use_module(user(user_db)).


/** <module> ClioPatria RDF data browser

This module implements basic browsing of an  RDF repository. This is not
intended to be used as an end-user application, but for the developer to
gain insight in the data in the   RDF  store. That said, the distinction
between end-user and developer  can  be   rather  vague  if  we consider
`back-office'   applications.   To   a   certain   extend,   back-office
applications  are  considered  within  the  scope  of  this  module  and
therefore it provides several  hooks   and  defines several `components'
that allow back-office applications to reuse this infrastructure.

@see    cliopatria(hooks) for available hooks.
*/

                 /*******************************
                 *            PATHS             *
                 *******************************/

:- http_handler(rdf_browser(.),
                http_404([index(list_graphs)]),
                [spawn(cliopatria), prefix]).
:- http_handler(rdf_browser(list_graphs),     list_graphs,     []).
:- http_handler(rdf_browser(list_graph),      list_graph,      []).
:- http_handler(rdf_browser(list_classes),    list_classes,    []).
:- http_handler(rdf_browser(list_instances),  list_instances,  []).
:- http_handler(rdf_browser(list_predicates), list_predicates, []).
:- http_handler(rdf_browser(list_predicate_resources),
                                              list_predicate_resources, []).
:- http_handler(rdf_browser(list_resource),   list_resource,   []).
:- http_handler(rdf_browser(list_triples),    list_triples,    []).
:- http_handler(rdf_browser(list_triples_with_object),
                                              list_triples_with_object, []).
:- http_handler(rdf_browser(list_triples_with_literal),
                                              list_triples_with_literal, []).

:- http_handler(rdf_browser(list_prefixes),   list_prefixes,   []).
:- http_handler(rdf_browser(search),          search,          []).
:- http_handler(rdf_browser(multigraph_action), multigraph_action,
                [ time_limit(infinite) ]).


:- meta_predicate
    table_rows(3, +, ?, ?),
    table_rows_top_bottom(3, +, +, +, ?, ?),
    html_property_table(?, 0, ?, ?).

%!  list_graphs(+Request)
%
%   Display a page holding a table with all RDF graphs.  The graphs
%   are sorted to the number of triples.

list_graphs(_Request) :-
    authorized_browse(default),
    findall(Count-Graph,
            (   rdf_graph(Graph),
                graph_triples(Graph, Count)
            ),
            Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, UpCount),
    reverse(UpCount, DownCount),
    append(DownCount, [virtual(total)], Rows),
    reply_html_page(cliopatria(default),
                    title('RDF Graphs'),
                    [ h1('Named graphs in the RDF store'),
                      \warn_volatile,
                      \graph_table(Rows, [])
                    ]).

:- if(current_predicate(rdf_persistency_property/1)).
warn_volatile -->
    { rdf_persistency_property(access(read_only)),
      !,
      rdf_persistency_property(directory(Dir))
    },
    html(div(class(msg_warning),
             [ 'WARNING: The persistent store ', code(Dir), ' was loaded in ',
               b('read-only'), ' mode.  All changes will be lost when ',
               'the server is stopped.'
             ])).
:- endif.
warn_volatile --> [].

:- if((rdf_version(V),V>=30000)).
graph_triples(Graph, Count) :-
    rdf_statistics(triples_by_graph(Graph, Count)).
:- else.
graph_triples(Graph, Count) :-                  % RDF-DB < 3.0
    rdf_statistics(triples_by_file(Graph, Count)).
:- endif.

graph_table(Graphs, Options) -->
    { option(top_max(TopMax), Options, 500),
      option(top_max(BottomMax), Options, 500),
      http_link_to_id(multigraph_action, [], Action),
      graph_actions(Options, ActionOptions)
    },
    html_requires(css('rdf.css')),
    html(form([ action(Action),
                class('graph-table')
              ],
              [ table(class(block),
                      [ \graph_table_header
                      | \table_rows_top_bottom(
                             graph_row(ActionOptions), Graphs,
                             TopMax, BottomMax)
                      ]),
                \multigraph_actions(ActionOptions)
              ])),
    mgraph_action_script.

graph_table_header -->
    html(tr([ th('RDF Graph'),
              th('Triples'),
              th('Modified'),
              th('Persistency')
            ])).

graph_row(_, virtual(total)) -->
    !,
    { rdf_statistics(triples(Count))
    },
    html([ th(class(total), 'Total #triples:'),
           \nc('~D', Count, [class(total)]),
           td([],[]),  % Empty cell for persistency column
           td([],[])   % Empty cell for modified column
         ]).
graph_row(Options, Graph) -->
    { graph_triples(Graph, Count)

    },
    html([ td(\graph_link(Graph)),
           \nc('~D', Count),
           \modified(Graph),
           td(style('text-align:center'), \persistency(Graph)),
           \graph_checkbox(Graph, Options)
         ]).

modified(Graph) -->
    { rdf_graph_property(Graph, source_last_modified(Time)),
      format_time(string(Modified), '%+', Time), !
    },
    html(td([class('file-time')], Modified)).
modified(Graph) -->
    { rdf_journal_file(Graph, File),
      time_file(File, Time),
      format_time(string(Modified), '%+', Time)
    },
    html(td([class('file-time')], Modified)).
modified(_Graph) -->
    html(td([class('file-time')], '')).

graph_link(Graph) -->
    { http_link_to_id(list_graph, [graph=Graph], URI)
    },
    html(a(href(URI), Graph)).

persistency(Graph) -->
    { rdf_graph_property(Graph, persistent(true)) },
    !,
    snapshot(Graph),
    journal(Graph).
persistency(_) -->
    { http_absolute_location(icons('volatile.png'), Img, [])
    },
    html(img([ class('in-text'),
               title('Graph is not persistent'),
               src(Img)
             ])).

snapshot(Graph) -->
    { rdf_snapshot_file(Graph, _),
      http_absolute_location(icons('snapshot.png'), Img, [])
    },
    html(img([ class('in-text'),
               title('Graph has persistent snapshot'),
               src(Img)
             ])).
snapshot(_) --> [].

journal(Graph) -->
    { rdf_journal_file(Graph, _),
      http_absolute_location(icons('journal.png'), Img, [])
    },
    html(img([ class('in-text'),
               title('Graph has a journal'),
               src(Img)
             ])).
journal(_) --> [].

%!  graph_actions(+Options0, -Options).
%!  multigraph_actions(+Options)
%
%   Deal with actions on multiple graphs.

graph_actions(Options, [show_actions(true)|Options]) :-
    logged_on(User),
    !,
    catch(check_permission(User, write(_, unload(user))), _, fail),
    !.
graph_actions(Options, Options).

graph_checkbox(Graph, Options) -->
    { option(show_actions(true), Options) },
    !,
    html(td(class('no-border'),
            input([type(checkbox),name(graph),value(Graph),
                   class('graph-select')]))).
graph_checkbox(_, _) --> [].

multigraph_actions(Options) -->
    { option(show_actions(true), Options),
      !,
      findall(Action-Format,
              clause(graph_action(Action,Format,_), _),
              Pairs)
    },
    html([ ul([ class('multi-graph-actions')
              ],
              \li_graph_actions(Pairs))
         ]).
multigraph_actions(_) --> [].

li_graph_actions([]) --> [].
li_graph_actions([H|T]) --> li_graph_action(H), li_graph_actions(T).

li_graph_action(Action-Format) -->
    { atomic_list_concat([Pre,Post], '~w', Format) },
    html(li([ Pre,
              input([ type(submit), name(action), value(Action) ]),
              Post
            ])).

mgraph_action_script -->
    html_requires(jquery),
    js_script({|javascript||
function showActions(time) {
  if ( time === undefined ) time = 400;
  var val = [];
  $('.graph-table :checkbox:checked').each(function(i) {
    val[i] = $(this).val();
  });
  if ( val.length == 0 )
    $(".multi-graph-actions").hide(time);
  else
    $(".multi-graph-actions").show(time);
}

$(function() {
  showActions(0);
  $(".graph-table .graph-select").on('click', showActions);
});
              |}).

%!  multigraph_action(Request)
%
%   HTTP Handler for user actions on multiple graphs.

multigraph_action(Request) :-
    findall(Action, clause(graph_action(Action,_,_), _), Actions),
    http_parameters(Request,
                    [ graph(Graphs, [list(atom)]),
                      action(Action, [oneof(Actions)])
                    ]),
    forall(member(Graph, Graphs),
           authorized(write(Graph, Action))),
    clause(graph_action(Action,Format,_), _),
    api_action(Request, multigraph_action(Action, Graphs), html,
               Format-[Action]).

multigraph_action(Action, Graphs) :-
    forall(member(Graph, Graphs),
           ( print_message(informational,
                           format('Processing ~w ...', [Graph])),
             graph_action(Action, _, Graph))).

graph_action('Delete', '~w selected graphs', Graph) :-
    rdf_unload_graph(Graph).
graph_action(volatile, 'Make selected graphs ~w', Graph) :-
    rdf_persistency(Graph, false).
graph_action(persistent, 'Make selected graphs ~w', Graph) :-
    rdf_persistency(Graph, true).
graph_action('Merge journals', '~w for selected graphs', Graph) :-
    rdf_flush_journals([graph(Graph)]).


%!  list_graph(+Request)
%
%   HTTP handler that provides information   about an individual RDF
%   graph. The output is an HTML table.

list_graph(Request) :-
    http_parameters(Request,
                    [ graph(Graph,
                            [description('Name of the graph to describe')])
                    ]),
    authorized_browse(Graph),
    (   rdf_graph(Graph)
    ->  true
    ;   http_404([], Request)
    ),
    reply_html_page(cliopatria(default),
                    title('RDF Graph ~w'-[Graph]),
                    [ h1('Summary information for graph "~w"'-[Graph]),
                      \simple_search_form([ id(ac_find_in_graph),
                                            filter(graph(Graph)),
                                            label('Search this graph')
                                          ]),
                      \graph_info(Graph),
                      \graph_as_resource(Graph, []),
                      \graph_persistency(Graph),
                      \graph_actions(Graph),
                      \uri_info(Graph, Graph)
                    ]).

%!  graph_info(+Graph)//
%
%   HTML component that shows  -statistical-   properties  about the
%   given named graph.

graph_info(Graph) -->
    html_property_table(row(P,V),
                        graph_property(Graph,P,V)).

:- dynamic
    graph_property_cache/3.

graph_property(Graph, P, V) :-
    graph_property_cache(Graph, MD5, Pairs),
    rdf_md5(Graph, MD5),
    !,
    member(P0-V, Pairs),
    P =.. [P0,Graph].
graph_property(Graph, P, V) :-
    retractall(graph_property_cache(Graph, _, _)),
    findall(P-V, graph_property_nc(Graph, P, V), Pairs),
    rdf_md5(Graph, MD5),
    assert(graph_property_cache(Graph, MD5, Pairs)),
    member(P0-V, Pairs),
    P =.. [P0,Graph].

graph_property_nc(Graph, source, Source) :-
    rdf_source(Graph, Source).
graph_property_nc(Graph, triples, int(Triples)) :-
    graph_triples(Graph, Triples).
graph_property_nc(Graph, predicate_count, int(Count)) :-
    aggregate_all(count, predicate_in_graph(Graph, _P), Count).
graph_property_nc(Graph, subject_count, int(Count)) :-
    aggregate_all(count, subject_in_graph(Graph, _P), Count).
graph_property_nc(Graph, bnode_count, int(Count)) :-
    aggregate_all(count, bnode_in_graph(Graph, _P), Count).
graph_property_nc(Graph, type_count, int(Count)) :-
    aggregate_all(count, type_in_graph(Graph, _P), Count).

predicate_in_graph(Graph, P) :-
    rdf_current_predicate(P),
    once(rdf(_,P,_,Graph)).

%!  subject_in_graph(+Graph, -Subject)
%
%   Generate the distinct subjects in a graph. There are two ways to
%   do this: first the subjects and then  whether they appear in the
%   graph or the other way around. At   least this has the advantage
%   that we get distinct subjects for free.

subject_in_graph(Graph, S) :-
    graph_triples(Graph, Count),
    rdf_statistics(triples(Total)),
    Count * 10 > Total,            % Graph has more than 10% of triples
    !,
    rdf_subject(S),
    once(rdf(S, _, _, Graph)).
subject_in_graph(Graph, S) :-
    findall(S, rdf(S,_,_,Graph), List),
    sort(List, Subjects),
    member(S, Subjects).

bnode_in_graph(Graph, S) :-
    graph_triples(Graph, Count),
    rdf_statistics(triples(Total)),
    Count * 10 > Total,
    !,
    rdf_subject(S),
    rdf_is_bnode(S),
    once(rdf(S, _, _, Graph)).
bnode_in_graph(Graph, S) :-
    findall(S, (rdf(S,_,_,Graph), rdf_is_bnode(S)), List),
    sort(List, Subjects),
    member(S, Subjects).

%!  type_in_graph(+Graph, -Class)
%
%   Generate the unique types in Graph

:- thread_local
    type_seen/1.

type_in_graph(Graph, Class) :-
    call_cleanup(type_in_graph2(Graph, Class),
                 retractall(type_seen(_))).

type_in_graph2(Graph, Class) :-
    subject_in_graph(Graph, S),
    (   rdf_has(S, rdf:type, Class)
    *-> true
    ;   rdf_equal(Class, rdfs:'Resource')
    ),
    (   type_seen(Class)
    ->  fail
    ;   assert(type_seen(Class))
    ).


%!  graph_persistency(+Graph)//
%
%   Show information about the persistency of the graph

graph_persistency(Graph) -->
    { rdf_graph_property(Graph, persistent(true)),
      (   rdf_journal_file(Graph, _)
      ;   rdf_snapshot_file(Graph, _)
      )
    },
    !,
    html([ h1('Persistency information'),
           table(class(block),
                 [ tr([ td(class('no-border'),[]),
                        th('File'), th('Size'),th('Modified'),
                        td(class('no-border'),[])
                      ]),
                   \graph_shapshot(Graph),
                   \graph_journal(Graph)
                 ])
         ]).
graph_persistency(Graph) -->
    { rdf_graph_property(Graph, persistent(true))
    },
    !,
    html([ h1('Persistency information'),
           p('The graph has no associated persistency files')
         ]).
graph_persistency(_Graph) -->
    [].

graph_shapshot(Graph) -->
    { rdf_snapshot_file(Graph, File)
    },
    html(tr([ th(class('file-role'), 'Snapshot'),
              \file_info(File)
            ])).
graph_shapshot(_) --> [].


graph_journal(Graph) -->
    { rdf_journal_file(Graph, File)
    },
    html(tr([ th(class('file-role'), 'Journal'),
              \file_info(File),
              \flush_journal_button(Graph)
            ])).
graph_journal(_) --> [].

flush_journal_button(Graph) -->
    { http_link_to_id(flush_journal, [], HREF)
    },
    html(td(class('no-border'),
            form(action(HREF),
                 [ input([type(hidden), name(graph), value(Graph)]),
                   input([type(hidden), name(resultFormat), value(html)]),
                   input([type(submit), value('Merge journal')])
                 ]))).


file_info(File) -->
    { size_file(File, Size),
      time_file(File, Time),
      format_time(string(Modified), '%+', Time)
    },
    html([ td(class('file-name'), File),
           td(class('int'), \n(human, Size)),
           td(class('file-time'), Modified)
         ]).


%!  graph_actions(+Graph)// is det.
%
%   Provide a form for basic actions on the graph

graph_actions(Graph) -->
    html([ h2('Actions'),
           ul(class(graph_actions),
              [ \li_export_graph(Graph, show),
                \li_export_graph(Graph, download),
                \li_schema_graph(Graph),
                \li_delete_graph(Graph),
                \li_persistent_graph(Graph)
              ])
         ]).

li_delete_graph(Graph) -->
    { logged_on(User),
      catch(check_permission(User, write(_, unload(Graph))), _, fail),
      !,
      http_link_to_id(unload_graph, [], Action)
    },
    html(li(form(action(Action),
                 [ input([type(hidden), name(graph), value(Graph)]),
                   input([type(hidden), name(resultFormat), value(html)]),
                   input([class(gaction), type(submit), value('Delete')]),
                   ' this graph'
                 ]))).
li_delete_graph(_) --> [].

li_persistent_graph(Graph) -->
    { logged_on(User),
      catch(check_permission(User, write(_, persistent(Graph))), _, fail),
      !,
      http_link_to_id(modify_persistency, [], Action),
      (   rdf_graph_property(Graph, persistent(true))
      ->  Op = (volatile),   Value = off
      ;   Op = (persistent), Value = on
      )
    },
    !,
    html(li(form(action(Action),
                 [ input([type(hidden), name(graph), value(Graph)]),
                   input([type(hidden), name(resultFormat), value(html)]),
                   input([type(hidden), name(persistent), value(Value)]),
                   'Make this graph ',
                   input([class(gaction), type(submit), value(Op)])
                 ]))).
li_persistent_graph(_) --> [].

li_schema_graph(Graph) -->
    { http_link_to_id(export_graph_schema, [], Action),
      download_options(show, Label, MimeType, Title)
    },
    html(li(form(action(Action),
                 [ input([type(hidden), name(graph), value(Graph)]),
                   input([type(hidden), name(mimetype), value(MimeType)]),
                   'Compute a schema for this graph and ',
                   input([class(saction), type(submit), value(Label),
                          title(Title)
                         ]),
                   ' the result as ',
                   \dl_format_menu
                 ]))).

li_export_graph(Graph, How) -->
    { http_link_to_id(export_graph, [], Action),
      download_options(How, Label, MimeType, Title)
    },
    html(li(form(action(Action),
                 [ input([type(hidden), name(graph), value(Graph)]),
                   input([type(hidden), name(mimetype), value(MimeType)]),
                   input([class(gaction), type(submit), value(Label),
                          title(Title)
                         ]),
                   ' this graph as ',
                   \dl_format_menu
                 ]))).

download_options(show,     'Show',     'text/plain',
                 'Returns graph with MIME-type text/plain, \n\c
                  so it will be displayed in your browser').
download_options(download, 'Download', default,
                 'Return graph with its RDF MIME-type, \n\c
                  so most browsers will save it').

dl_format_menu -->
    html(select(name(format),
                [ option([value(turtle),selected],  'Turtle'),
                  option([value(canonical_turtle)], 'Canonical Turtle'),
                  option([value(rdfxml)],           'RDF/XML')
                ])).


%!  list_classes(+Request)
%
%   HTTP handler that lists all classes  of all subjects that appear
%   in the named graph. The  output  is   an  HTML  page holding all
%   referenced classes sorted by their label.

list_classes(Request) :-
    http_parameters(Request,
                    [ graph(Graph, [description('Name of the graph')])
                    ]),
    authorized_browse(Graph),
    types_in_graph(Graph, Map),
    sort_pairs_by_label(Map, Sorted),
    reply_html_page(cliopatria(default),
                    title('Classes in graph ~w'-[Graph]),
                    [ h1(['Classes in graph ', \graph_link(Graph)]),
                      \class_table(Sorted, Graph, [])
                    ]).

class_table(Pairs, Graph, Options) -->
    { option(top_max(TopMax), Options, 500),
      option(top_max(BottomMax), Options, 500)
    },
    html_requires(css('rdf.css')),
    html(table(class(block),
               [ \class_table_header
               | \table_rows_top_bottom(class_row(Graph), Pairs,
                                        TopMax, BottomMax)
               ])).

class_table_header -->
    html(tr([ th('Class'),
              th('#Instances')
            ])).

class_row(Graph, Class) -->
    { atom(Class),
      !,
      findall(I, rdf_has(I, rdf:type, Class, Graph), IL),
      sort(IL, Classes),
      length(Classes, InstanceCount)
    },
    class_row(Graph, Class-InstanceCount).
class_row(Graph, Class-InstanceCount) -->
    { (   var(Graph)
      ->  Params = [class(Class)]
      ;   Params = [graph(Graph), class(Class)]
      ),
      http_link_to_id(list_instances, Params, ILink)
    },
    html([ td(\rdf_link(Class, [role(class)])),
           td(class(int), a(href(ILink), InstanceCount))
         ]).

%!  types_in_graph(+Graph, -Map:list(Type-InstanceCount))
%
%   Generate a map of all types that appear in Graph with a count on
%   the number of instances.

types_in_graph(Graph, Map) :-
    findall(S, subject_in_graph(Graph, S), Subjects),
    types(Subjects, Pairs),
    transpose_pairs(Pairs, TypeSubj),
    group_pairs_by_key(TypeSubj, TypeSubjs),
    maplist(instance_count, TypeSubjs, Map).

types([], []).
types([S|T0], Types) :-
    call_det(type_of(S,C), Det),
    !,
    (   Det == true
    ->  Types = [S-C|T],
        types(T0, T)
    ;   findall(C2, type_of(S,C2), Cs),
        multi_class(Cs, S, Types, PT),
        types(T0, PT)
    ).

multi_class([], _, Pairs, Pairs).
multi_class([H|T], S, [S-H|Pairs], PT) :-
    multi_class(T, S, Pairs, PT).


type_of(Subject, Type) :-
    (   rdf_has(Subject, rdf:type, Type)
    *-> true
    ;   rdf_equal(Type, rdfs:'Resource')
    ).

:- meta_predicate
    call_det(0, -).

call_det(G, Det) :-
    call(G),
    deterministic(Det).

instance_count(Type-Instances, Type-Count) :-
    length(Instances, Count).

%!  instance_in_graph(?Graph, ?Class, +Type,
%!                    -Subject, -PropertyCount) is nondet.
%
%   True of Subject is  an  instance   of  Class  with PropertyCount
%   properties provided from Graph.

instance_in_graph(Graph, Class, any, S, C) :-
    !,
    instance_in_graph(Graph, Class, S, C).
instance_in_graph(Graph, Class, bnode, S, C) :-
    !,
    freeze(S, rdf_is_bnode(S)),
    instance_in_graph(Graph, Class, S, C).


instance_in_graph(Graph, Class, S, C) :-
    var(Class),
    !,
    subject_in_graph(Graph, S),
    property_count(Graph, S, C).
instance_in_graph(Graph, Class, S, C) :-
    rdf_equal(Class, rdfs:'Resource'),
    !,
    (   rdf_has(S, rdf:type, Class),
        once(rdf(S, _, _, Graph))
    ;   subject_in_graph(Graph, S),
        \+ rdf_has(S, rdf:type, _)
    ),
    property_count(Graph, S, C).
instance_in_graph(Graph, Class, S, C) :-
    rdf_has(S, rdf:type, Class),
    once(rdf(S, _, _, Graph)),
    property_count(Graph, S, C).

property_count(Graph, S, Count) :-
    aggregate_all(count, rdf(S, _, _, Graph), Count).

%!  graph_as_resource(+Graph, Options)// is det.
%
%   Show resource info for a graph if it is described.

graph_as_resource(Graph, Options) -->
    { (   rdf(Graph, _, _)
      ;   rdf(_, Graph, _)
      ;   rdf(_, _, Graph)
      ), !
    },
    html([ h2([ 'Local view for "',
                \location(Graph, _), '"'
              ]),
           \local_view(Graph, _, Options)
         ]).
graph_as_resource(_, _) --> [].


                 /*******************************
                 *        LIST INSTANCES        *
                 *******************************/

%!  list_instances(+Request)
%
%   HTTP handler that lists instances that satisfy certain criteria.

list_instances(Request) :-
    http_parameters(Request,
                    [ class(Class,
                            [ optional(true),
                              description('Limit to instances of this class')
                            ]),
                      graph(Graph,
                            [ optional(true),
                              description('Limit to have at least \c
                                               one property in graph')
                            ]),
                      type(Type,
                           [ oneof([any, bnode]),
                             default(any),
                             description('Any instance or only bnodes?')
                           ]),
                      resource_format(Format,
                            [ optional(true),
                              atom,
                              description('Display format as passed to rdf_link//2 ')
                            ]),
                      sortBy(Sort,
                             [ oneof([label,properties]),
                               default(label),
                               description('How to sort the result-table')
                             ])
                    ]),
    include(ground,
            [ class(Class),
              graph(Graph),
              type(Type),
              resource_format(Format),
              sort_by(Sort)
            ], Options),

    authorized_browse(Graph),
    instance_table_title(Options, Title),
    reply_html_page(cliopatria(default),
                    title(Title),
                    [ h1(\html_instance_table_title(Options)),
                      \instance_table(Options)
                    ]).

instance_table_title(Options, Title) :-
    phrase(html_instance_table_title(Options), Tokens),
    with_output_to(string(HTML), print_html(Tokens)),
    setup_call_cleanup(
        open_string(HTML, In),
        load_html(stream(In), DOM, []),
        close(In)),
    xpath(element(title, [], DOM), //title(text(string)), Title).

html_instance_table_title(Options) -->
    html([ 'Instances',
           \of_class(Options),
           \in_graph(Options),
           \sortBy(Options)
         ]).

of_class(Options) -->
    { option(class(Class), Options),
      nonvar(Class)
    },
    !,
    html([' of class ', \rdf_link(Class, [role(class)])]).
of_class(_) -->
    [].

in_graph(Options) -->
    { option(graph(Graph), Options),
      nonvar(Graph)
    },
    !,
    html([' in graph ', \graph_link(Graph)]).
in_graph(_) -->
    [].

sortBy(Options) -->
    { option(sort_by(Sort), Options, label) },
    html(' sorted by ~w'-[Sort]).

%!  instance_table(+Options)// is det.
%
%   Show a table with instances according to Options.

instance_table(Options) -->
    { setting(resource_format, DefaultFormat),
      option(graph(Graph), Options, _),
      option(class(Class), Options, _),
      option(type(Type), Options, any),
      option(resource_format(Format), Options, DefaultFormat),
      option(sort_by(Sort), Options, label),

      findall(I-PC, instance_in_graph(Graph, Class, Type, I, PC), IPairs),
      sort_pairs_by_label(IPairs, TableByName),
      (   Sort == properties
      ->  reverse(TableByName, RevTableByName),
          transpose_pairs(RevTableByName, FPairsUp),
          reverse(FPairsUp, FPairsDown),
          flip_pairs(FPairsDown, Table)
      ;   Table = TableByName
      )
    },
    instance_table(Table, [resource_format(Format)]).


instance_table(Pairs, Options) -->
    { option(top_max(TopMax), Options, 500),
      option(top_max(BottomMax), Options, 500)
    },
    html_requires(css('rdf.css')),
    html(table(class(block),
               [ \instance_table_header
               | \table_rows_top_bottom(instance_row(Options), Pairs,
                                        TopMax, BottomMax)
               ])).

instance_table_header -->
    html(tr([ th('Instance'),
              th('#Properties')
            ])).

instance_row(Options, R-C) -->
    html([ td(\rdf_link(R, [role(inst)|Options])),
           td(class(int), C)
         ]).


                 /*******************************
                 *           PREDICATES         *
                 *******************************/

%!  list_predicates(+Request)
%
%   List all predicates used in graph, sorted by label.

list_predicates(Request) :-
    http_parameters(Request,
                    [ graph(Graph, [])
                    ]),
    authorized_browse(Graph),
    findall(Pred, predicate_in_graph(Graph, Pred), Preds),
    sort_by_label(Preds, Sorted),
    reply_html_page(cliopatria(default),
                    title('Predicates in graph ~w'-[Graph]),
                    [ h1(['Predicates in graph ', \graph_link(Graph)]),
                      \predicate_table(Sorted, Graph, [])
                    ]).

predicate_table(Preds, Graph, Options) -->
    { option(top_max(TopMax), Options, 500),
      option(bottom_max(BottomMax), Options, 500)
    },
    html_requires(css('rdf.css')),
    html(table(class(block),
               [ \predicate_table_header
               | \table_rows_top_bottom(predicate_row(Graph), Preds,
                                        TopMax, BottomMax)
               ])).

predicate_table_header -->
    html(tr([ th('Predicate'),
              th('#Triples'),
              th('#Distinct subjects'),
              th('#Distinct objects'),
              th('Domain(s)'),
              th('Range(s)')
            ])).

%!  predicate_row(?Graph, +Pred) is det.

predicate_row(Graph, Pred) -->
    { predicate_statistics(Graph, Pred, Triples,
                           Subjects, Objects, Doms, Ranges),
      (   var(Graph)
      ->  Params = [predicate(Pred)]
      ;   Params = [graph(Graph), predicate(Pred)]
      ),
      http_link_to_id(list_triples,   Params, PLink)
    },
    html([ td(\rdf_link(Pred, [role(pred)])),
           td(class(int), a(href(PLink), Triples)),
           \resources(Subjects, subject, Params, [role(subj)]),
           \resources(Objects, object, Params, [role(obj)]),
           \resources(Doms, domain, Params, [role(domain)]),
           \resources(Ranges, range, Params, [role(range)])
         ]).

resources([], _, _, _) -->
    !,
    html(td(class(empty), -)).
resources([One], _, _, Options) -->
    !,
    html(td(\rdf_link(One, Options))).
resources(Many, What, Params, _) -->
    !,
    { (   integer(Many)
      ->  Count = Many
      ;   length(Many, Count)
      ),
      http_link_to_id(list_predicate_resources, [side(What)|Params], Link)
    },
    html(td(class(int_c), a(href(Link), Count))).

:- dynamic
    predicate_statistics_cache/8.

predicate_statistics(Graph, P, C, Subjects, Objects, Domains, Ranges) :-
    var(Graph),
    !,
    predicate_statistics_(Graph, P, C, Subjects, Objects, Domains, Ranges).
predicate_statistics(Graph, P, C, Subjects, Objects, Domains, Ranges) :-
    rdf_md5(Graph, MD5),
    predicate_statistics_cache(MD5, Graph, P, C,
                               Subjects, Objects, Domains, Ranges),
    !.
predicate_statistics(Graph, P, C, Subjects, Objects, Domains, Ranges) :-
    rdf_md5(Graph, MD5),
    debug(rdf_browse, 'Recomputing pred stats for ~p in ~w, MD5=~w',
          [P, Graph, MD5]),
    retractall(predicate_statistics_cache(MD5, Graph, P, _,
                                          _, _, _, _)),
    predicate_statistics_(Graph, P, C, SubjectL, ObjectL, DomainL, RangeL),
    res_summary(SubjectL, Subjects),
    res_summary(ObjectL, Objects),
    res_summary(DomainL, Domains),
    res_summary(RangeL, Ranges),
    assertz(predicate_statistics_cache(MD5, Graph, P, C,
                                       Subjects, Objects, Domains, Ranges)).


res_summary([], []) :- !.
res_summary([One], [One]) :- !.
res_summary(Many, Count) :-
    length(Many, Count).


predicate_statistics_(Graph, P, C, Subjects, Objects, Domains, Ranges) :-
    findall(S-O, rdf(S,P,O,Graph), Pairs),
    length(Pairs, C),
    pairs_keys_values(Pairs, Ss, Os),
    sort(Ss, Subjects),
    sort(Os, Objects),
    resources_types(Subjects, Graph, Domains),
    resources_types(Objects, Graph, Ranges).

resources_types(URIs, Graph, Types) :-
    findall(T, resource_type_in(URIs, Graph, T), TList),
    sort(TList, Types).

resource_type_in(List, Graph, T) :-
    member(URI, List),
    resource_type(URI, Graph, T).

%!  resource_type(+URI, +Graph, -Type) is multi.

resource_type(literal(Lit), _, Type) :-
    !,
    (   Lit = type(Type, _)
    ->  true
    ;   rdf_equal(Type, rdfs:'Literal')
    ).
resource_type(^^(_, Type0), _, Type) :-
    !,
    Type = Type0.
resource_type(@(_,_), _, Type) :-
    !,
    rdf_equal(Type, rdf:langString).
resource_type(URI, Graph, Type) :-
    (   string(URI)
    ->  rdf_equal(Type, xsd:string)
    ;   rdf(URI, rdf:type, Type, Graph)
    *-> true
    ;   rdf_equal(Type, rdfs:'Resource')
    ).


                 /*******************************
                 *        LIST RESOURCES        *
                 *******************************/

%!  list_predicate_resources(+Request)
%
%   List resources related to a predicate.   The  _side_ argument is
%   one of:
%
%       * subject
%       Display all subject values for the predicate
%       * object
%       Display all object values for the predicate
%       * domain
%       Display the types of all subject values
%       * range
%       Display the types of all object values.
%
%   If the _skosmap_ attribute is =true=,   an extra column is added
%   that shows SKOS concepts that match   literals.  This only makes
%   sense if _side_ = =object= and (some) objects are literals.

list_predicate_resources(Request) :-
    http_parameters(Request,
                    [ graph(Graph,
                            [ optional(true),
                              description('Limit search to this graph')
                            ]),
                      predicate(Pred,
                                [ description('Predicate to list')
                                ]),
                      side(Which,
                           [ oneof([subject,object,domain,range]),
                             description('Relation to the predicate (see docs)')
                           ]),
                      sortBy(Sort,
                             [ oneof([label,frequency]),
                               default(frequency),
                               description('How to sort results')
                             ]),
                      skosmap(SkosMap,
                              [ boolean,
                                optional(true),
                                description('Show SKOS concepts for literals')
                              ])
                    ]),
    authorized_browse(Graph),
    do_skos(SkosMap, Which, Pred),
    findall(R, predicate_resource(Graph, Pred, Which, R), Set),
    term_frequency_list(Set, FPairs),
    sort_pairs_by_label(FPairs, TableByName),
    (   Sort == frequency
    ->  reverse(TableByName, RevTableByName),
        transpose_pairs(RevTableByName, FPairsUp),
        reverse(FPairsUp, FPairsDown),
        flip_pairs(FPairsDown, Table)
    ;   Table = TableByName
    ),

    pred_resource_options(Pred, Which, Options),

    reply_html_page(cliopatria(default),
                    title(\resource_table_title(Graph, Pred, Which, Sort)),
                    [ h1(\html_resource_table_title(Graph, Pred, Which,
                                                    Sort, SkosMap)),
                      \resource_frequency_table(Table,
                                                [ skosmap(SkosMap),
                                                  predicate(Pred),
                                                  side(Which),
                                                  sort(Sort)
                                                | Options
                                                ])
                    ]).

pred_resource_options(_, domain, [label('Class')]) :- !.
pred_resource_options(_, range, [label('Class')]) :- !.
pred_resource_options(_, _, []).

do_skos(SkosMap, _, _) :-
    nonvar(SkosMap),
    !.
do_skos(SkosMap, object, Pred) :-
    \+ rdf(_, Pred, literal(_)),
    !,
    SkosMap = false.
do_skos(SkosMap, object, _) :-
    rdfs_individual_of(_, skos:'ConceptScheme'),
    !,
    SkosMap = true.
do_skos(false, _, _).


resource_table_title(Graph, Pred, Which, Sort) -->
    { rdf_display_label(Pred, PLabel)
    },
    html('Distinct ~ws for ~w in ~w sorted by ~w'-
         [Which, PLabel, Graph, Sort]
         ).

html_resource_table_title(Graph, Pred, Which, Sort, SkosMap) -->
    html([ 'Distinct ~ws'-[Which],
           \for_predicate(Pred),
           \in_graph([graph(Graph)]),
           \sortBy([sortBy(Sort)]),
           \showing_skosmap(SkosMap)
         ]).

for_predicate(Pred) -->
    { var(Pred) },
    !.
for_predicate(Pred) -->
    html([' for predicate ', \rdf_link(Pred, [role(pred)])]).

showing_skosmap(true) -->
    !,
    html(' with mapping to SKOS').
showing_skosmap(_) --> [].

resource_frequency_table(Pairs, Options) -->
    { option(top_max(TopMax), Options, 500),
      option(top_max(BottomMax), Options, 500),
      option(predicate(Pred), Options, _),
      option(side(Side), Options)
    },
    html_requires(css('rdf.css')),
    html(table(class(block),
               [ \resource_table_header(Options)
               | \table_rows_top_bottom(resource_row(Pred, Side, [role(pred)|Options]), Pairs,
                                        TopMax, BottomMax)
               ])).

resource_table_header(Options) -->
    { option(label(Label), Options, 'Resource'),
      (   option(sort(Sort), Options)
      ->  (   Sort == frequency
          ->  A1 = [],
              A2 = [class(sorted)]
          ;   A1 = [class(sorted)],
              A2 = []
          )
      ;   A1 = [],
          A2 = []
      )
    },
    html(tr([ th(A1, Label),
              th(A2, 'Count'),
              \skosmap_head(Options)
            ])).

skosmap_head(Options) -->
    { option(skosmap(true), Options) },
    !,
    html(th('SKOS mapping')).
skosmap_head(_) --> [].

resource_row(Pred, object, Options, R-C) -->
    !,
    { object_param(R, Param),
      http_link_to_id(list_triples_with_object,
           [ p(Pred),
             Param
           ], HREF)
    },
    html([ td(\rdf_link(R, Options)),
           td(class(int), a(href(HREF), C)),
           \skosmap(R, Options)
         ]).
resource_row(Pred, Side, Options, R-C) -->
    { domain_range_parameter(Side, R, Param),
      !,
      http_link_to_id(list_triples,
           [ predicate(Pred),
             Param
           ], HREF)
    },
    html([ td(\rdf_link(R, Options)),
           td(class(int), a(href(HREF), C)),
           \skosmap(R, Options)
         ]).
resource_row(_, _, Options, R-C) -->
    html([ td(\rdf_link(R, Options)),
           td(class(int), C),
           \skosmap(R, Options)
         ]).

object_param(R, r=R) :-
    atom(R),
    !.
object_param(L, l=A) :-
    term_to_atom(L, A).

domain_range_parameter(domain, R, domain(R)).
domain_range_parameter(range,  R, range(R)).

%!  skosmap(+Literal, +Options)//
%
%   Component that emits a =td= cell with links to SKOS concepts
%   that are labeled Literal.

skosmap(Literal, Options) -->
    { Literal = literal(_),
      option(skosmap(true), Options),
      findall(Concept-Scheme, skos_find(Literal, Concept, Scheme), Pairs),
      Pairs \== [],
      sort_pairs_by_label(Pairs, Sorted)
    },
    html(td(\skos_references(Sorted))).
skosmap(_, _) --> [].

skos_find(Literal, Concept, Scheme) :-
    rdf_has(Concept, skos:prefLabel, Literal),
    rdf_has(Concept, skos:inScheme, Scheme).

skos_references([]) --> [].
skos_references([H|T]) -->
    skos_reference(H),
    (   { T == [] }
    ->  []
    ;   html('; '),
        skos_references(T)
    ).

skos_reference(Concept-Scheme) -->
    html([\rdf_link(Concept, [role(concept)]), ' in ', \rdf_link(Scheme, [role(scheme)])]).


flip_pairs([], []).
flip_pairs([Key-Val|Pairs], [Val-Key|Flipped]) :-
    flip_pairs(Pairs, Flipped).

predicate_resource(Graph, Pred, subject, R) :-
    !,
    rdf(R, Pred, _, Graph).
predicate_resource(Graph, Pred, object, R) :-
    !,
    rdf(_, Pred, R, Graph).
predicate_resource(Graph, Pred, domain, D) :-
    !,
    rdf(R, Pred, _, Graph),
    rdf(R, rdf:type, D, Graph).
predicate_resource(Graph, Pred, range, R) :-
    rdf(_, Pred, O, Graph),
    resource_type(O, Graph, R).

%!  term_frequency_list(+Terms, -TermFrequencyPairs)
%
%   TermFrequencyPairs is a list if  pairs Value-Count of equivalent
%   term in Terms.  Equivalence is determined using ==/2.  The terms
%   themselves are sorted on the standard order of terms.

term_frequency_list(Resources, Pairs) :-
    msort(Resources, Sorted),
    fpairs(Sorted, Pairs).

fpairs([], []).
fpairs([H|T0], [H-C|T]) :-
    pick_same(T0, T1, H, 1, C),
    fpairs(T1, T).

pick_same([H1|T0], L, H, F0, F) :-
    H == H1,
    !,
    F1 is F0 + 1,
    pick_same(T0, L, H, F1, F).
pick_same(L, L, _, F, F).


                 /*******************************
                 *    LIST A SINGLE RESOURCE    *
                 *******************************/

%!  list_resource(+Request)
%
%   HTTP handler that lists the property table for a single resource
%   (=local view)
%
%   @see    The functionality of this handler is also available as
%           an embedable component through list_resource//2.

list_resource(Request) :-
    http_parameters(Request,
                    [ r(URI,
                        [ description('URI to describe')]),
                      sorted(Sorted,
                             [ oneof([default,none]),
                               default(default),
                               description('How to sort properties')
                             ]),
                      graph(Graph,
                            [ optional(true),
                              description('Limit to properties from graph')
                            ]),
                      resource_format(Format,
                            [ default(DefaultFormat),
                              atom,
                              description('Display format as passed to rdf_link//2 ')
                            ]),
                      raw(Raw,
                          [ default(false),
                            boolean,
                            description('If true, omit application hook')
                          ])
                    ]),
    authorized_browse(Graph),
    setting(resource_format, DefaultFormat),
    rdf_display_label(URI, Label),
    reply_html_page(cliopatria(default),
                    title('Resource ~w'-[Label]),
                    \list_resource(URI,
                                   [ graph(Graph),
                                     sorted(Sorted),
                                     raw(Raw),
                                     resource_format(Format)
                                   ])).

%!  list_resource(+URI, +Options)// is det.
%
%   Component that emits the `local view'   for  URI. The local view
%   shows the basic properties  of  URI,   the  context  in which is
%   appears and the graphs from which the information is extracted.
%   Options is one of:
%
%       * graph(Graph)
%       Limit properties in the table to the given graph
%       * sorted(Sorted)
%       One of =default= or =none=.
%
%   Calls the hook cliopatria:list_resource//2.  For compatibility
%   reasons, it also tries the hook cliopatria:list_resource//1.
%
%   @see    list_resource/1 is the corresponding HTTP handler.  The
%           component rdf_link//1 creates a link to list_resource/1.

:- multifile
    cliopatria:list_resource//1,
    cliopatria:list_resource//2.

list_resource(URI, _Options) -->
    { \+ rdf(URI, _, _),
      \+ rdf(_, URI, _),
      \+ rdf(_, _, URI),
      \+ rdf(_, _, _, URI)
    },
    !,
    { http_current_request(Request),
      http_404([], Request)
    },
    html([ h1('Unknown URI'),
           p(['The URI does not appear in the graph, \c
              neither as subject, predicate, object or graph.'])
         ]).
list_resource(URI, Options) -->
    { \+ option(raw(true), Options) },
    (   cliopatria:list_resource(URI, Options)
    ->  []
    ;   cliopatria:list_resource(URI) % deprecated
    ).
list_resource(URI, Options) -->
    { option(graph(Graph), Options, _)
    },
    html([ h1([ 'Local view for "',
                \location(URI, Graph), '"'
              ]),
           \define_prefix(URI),
           \local_view(URI, Graph, Options),
           p(\as_object(URI, Graph)),
           p(\as_graph(URI)),
           \uri_info(URI, Graph)
         ]).

%!  define_prefix(+URI)//
%
%   Allow defining a new prefix if the  resource is not covered by a
%   prefix.

define_prefix(URI) -->
    { rdf_global_id(_Prefix:_Local, URI) },
    !.
define_prefix(URI) -->
    { iri_xml_namespace(URI, Namespace, LocalName),
      LocalName \== '',
      http_link_to_id(add_prefix, [], Action)
    },
    html(form(action(Action),
              ['No prefix for ', a(href(Namespace),Namespace), '. ',
               \hidden(uri, Namespace),
               input([name(prefix), size(8),
                      title('Short unique abbreviation')
                     ]),
               input([type(submit), value('Add prefix')])
              ])).
define_prefix(_) -->                    % Not a suitable URI.  Warn?
    [].


%!  location(+URI, ?Graph) is det.
%
%   Show the URI. If the URI is a blank node, show its context using
%   Turtle notation.

location(URI, _Graph) -->
    { rdf_is_bnode(URI),
      !,
      findall(Path, path_to_non_bnode(URI, Path), Paths),
      sort_by_length(Paths, PathsByLen),
      partition(starts_bnode, PathsByLen, StartsBNode, StartsReal),
      (   StartsReal = [Path|_]
      ->  true
      ;   last(StartsBNode, Path)
      )
    },
    bnode_location(Path).
location(URI, _) -->
    html(URI).

bnode_location([P-URI]) -->
    !,
    html([ '[', \rdf_link(P,  [role(pred)]), ' ',
                \rdf_link(URI,[role(bnode)]),
           ']'
         ]).
bnode_location([P-URI|More]) -->
    !,
    html([ '[', div(class(bnode_attr),
                    [ div(\rdf_link(P,  [ role(pred)])),
                      div(\rdf_link(URI,[ role(bnode)]))
                    ]), ' ',
           \bnode_location(More),
           ']'
         ]).
bnode_location([URI|More]) -->
    !,
    rdf_link(URI, [role(subj)]),
    html(' '),
    bnode_location(More).
bnode_location([]) -->
    [].

path_to_non_bnode(URI, Path) :-
    path_to_non_bnode_rev(URI, [URI], RevPath),
    reverse(RevPath, Path).

path_to_non_bnode_rev(URI, Seen, [P-URI|Path]) :-
    (   rdf_is_bnode(URI),
        rdf(S, P, URI),
        \+ memberchk(S, Seen)
    *-> path_to_non_bnode_rev(S, [S|Seen], Path)
    ;   fail
    ).
path_to_non_bnode_rev(URI, _, [URI]).

starts_bnode([URI|_]) :-
    rdf_is_bnode(URI).

sort_by_length(ListOfLists, ByLen) :-
    map_list_to_pairs(length, ListOfLists, Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, ByLen).

%!  as_graph(+URI) is det.
%
%   Show the places where URI is used as a named graph

as_graph(URI) --> { \+ rdf_graph(URI) }, !.
as_graph(URI) -->
    html([ 'This resource is also a ',
           a([href(location_by_id(list_graph)+'?graph='+encode(URI))],
             'named graph'),
           '.']).


%!  as_object(+URI, +Graph) is det.
%
%   Show the places where URI is used as an object.

as_object(URI, Graph) -->
    { findall(S-P, rdf(S,P,URI,Graph), Pairs),
      sort(Pairs, Unique)
    },
    as_object_locations(Unique, URI, Graph).

as_object_locations([], _URI, _) -->
    !,
    html([ 'The resource does not appear as an object' ]).
as_object_locations([S-P], URI, _) -->
    !,
    html([ 'The resource appears as object in one triple:',
           blockquote(class(triple),
                      [ '{ ',
                        \rdf_link(S, [role(subj)]), ', ',
                        \rdf_link(P, [role(pred)]), ', ',
                        \rdf_link(URI, [role(obj)]),
                        ' }'
                      ])
         ]).
as_object_locations(List, URI, Graph) -->
    !,
    { length(List, Len),
      (   var(Graph)
      ->  Extra = []
      ;   Extra = [graph=Graph]
      ),
      http_link_to_id(list_triples_with_object, [r=URI|Extra], Link)
    },
    html([ 'The resource appears as object in ',
           a(href(Link), [Len, ' triples'])
         ]).

%!  local_view(+URI, ?Graph, +Options) is det.
%
%   Show the local-view table for URI.  If Graph is given, only show
%   triples from the given graph.  Options processed:
%
%       * top_max(+Count)
%       * bottom_max(+Count)
%       * sorted(+How)
%       Defines the order of the predicates. One of =none=
%       (database order) or =default=
%       * show_graph(+Bool)
%
%   In addition, Options are passed to rdf_link//2.

local_view(URI, Graph, Options) -->
    { option(top_max(TopMax), Options, 500),
      option(bottom_max(BottomMax), Options, 500),
      po_pairs(URI, Graph, Pairs, Options),
      lview_graphs(URI, Graph, Graphs)
    },
    (   { Pairs \== []
        }
    ->  html_requires(css('rdf.css')),
        html(table(class(block),
                   [ \lview_header(Options)
                   | \table_rows_top_bottom(lview_row(Options, URI, Graphs),
                                            Pairs,
                                            TopMax, BottomMax)
                   ])),
        graph_footnotes(Graphs, Options)
    ;   { lod_uri_graph(URI, LODGraph),
          rdf_graph(LODGraph)
        }
    ->  html(p([ 'No triples for ', \show_link(URI), '. ',
                 'Linked Data was loaded into ', \graph_link(LODGraph),
                 '.'
               ]))
    ;   { sane_uri(URI) }
    ->  { http_link_to_id(lod_crawl, [], FetchURL),
          http_current_request(Request),
          memberchk(request_uri(Here), Request),
          string_concat(URI, '.rdf', RealURL)
        },
        html(form(action(FetchURL),
                  [ \hidden(r, RealURL),
                    \hidden(return_to, Here),
                    'No triples for ', \show_link(URI),
                    '.  Would you like to ',
                    input([ type(submit),
                            value('Query the Linked Data cloud')
                          ]),
                    '?'
                  ]))
    ;   html_requires(css('rdf.css')),
        html(p([ 'No triples for ', \show_link(URI),
                 ' (unknown URI scheme).']))
    ).

show_link(URI) -->
    { sane_uri(URI) },
    !,
    html(a(href(URI), 'this URI')).
show_link(URI) -->
    html(span(class('insecure-uri'), URI)).

sane_uri(URI) :-
    uri_components(URI, Components),
    uri_data(scheme, Components, Scheme),
    valid_scheme(Scheme),
    uri_data(authority, Components, Authority),
    nonvar(Authority).

valid_scheme(http).
valid_scheme(https).
valid_scheme(ftp).
valid_scheme(ftps).

lview_header(Options) -->
    { option(sorted(Sorted), Options, default),
      alt_sorted(Sorted, Alt),
      http_current_request(Request),
      http_reload_with_parameters(Request, [sorted(Alt)], HREF)
    },
    html(tr([ th('Predicate'),
              th(['Value (sorted: ', a(href(HREF), Sorted), ')'])
            ])).

alt_sorted(default, none).
alt_sorted(none, default).


lview_row(Options, S, Graphs, P-OList) -->
    html([ td(class(predicate), \rdf_link(P, [role(pred)|Options])),
           td(class(object), \object_list(OList, S, P, Graphs, Options, 1))
         ]).

object_list([], _, _, _, _, _) --> [].
object_list([H|T], S, P, Graphs, Options, Row) -->
    { NextRow is Row + 1,
      obj_class(Row, Class)
    },
    html(div(class(Class),
             [ \rdf_link(H, [role(obj)|Options]),
               \graph_marks(S, P, H, Graphs)
             ])),
    object_list(T, S, P, Graphs, Options, NextRow).

obj_class(N, Class) :-
    (   N mod 2 =:= 0
    ->  Class = even
    ;   Class = odd
    ).

graph_marks(_,_,_,[_]) --> !.
graph_marks(S,P,O,Graphs) -->
    html(sup(class(graph), \graphs(S,P,O,Graphs))).

graphs(S, P, O, Graphs) -->
    { findall(G, rdf(S,P,O,G:_), GL) },
    graphs(GL, Graphs).

graphs([], _) --> [].
graphs([H|T], Graphs) -->
    { nth1(N, Graphs, H) -> true },
    html(N),
    (   { T == [] }
    ->  []
    ;   html(','),
        graphs(T, Graphs)
    ).

%!  graph_footnotes(+GraphList, +Options)//
%
%   Describe footnote marks in the local   view  table that indicate
%   the origin of triples.

graph_footnotes([], _Options) --> !.
graph_footnotes([Graph], _Options) -->
    !,
    html(p(class('graphs-used'),
           [ 'All properties reside in the graph ',
             \graph_link(Graph)
           ])).
graph_footnotes(Graphs, Options) -->
    html(p(class('graphs-used'),
           'Named graphs describing this resource:')),
    graph_footnotes(Graphs, 1, Options).

graph_footnotes([], _, _) --> [].
graph_footnotes([H|T], N, Options) -->
    html(div(class('graph-fn'),
             [ sup(class(graph), N),
               \graph_link(H)
             ])),
    { N2 is N + 1 },
    graph_footnotes(T, N2, Options).

%!  lview_graphs(+Subject, ?Graph, -Graphs) is det.

lview_graphs(_Subject, Graph, Graphs) :-
    nonvar(Graph),
    !,
    Graphs = [Graph].
lview_graphs(Subject, Graph, Graphs) :-
    findall(Graph, rdf(Subject, _, _, Graph:_), Graphs0),
    sort(Graphs0, Graphs).

%!  po_pairs(+Subject, ?Graph, -Pairs, +Options) is det.
%
%   Pairs is a  list  of  P-ObjectList   for  the  S,P,O  triples on
%   Subject. The list is normally sorted  by predicate as defined by
%   p_order/2 below.

po_pairs(S, Graph, Pairs, Options) :-
    option(sorted(none), Options),
    !,
    findall(P-[O], rdf(S,P,O,Graph), Pairs).
po_pairs(S, Graph, Pairs, _Options) :-
    var(Graph),
    !,
    findall(P-OL,
            setof(O, rdf(S,P,O), OL),
            Pairs0),
    sort_po(Pairs0, Pairs).
po_pairs(S, Graph, Pairs, _Options) :-
    findall(P-OL,
            setof(O, rdf(S,P,O,Graph), OL),
            Pairs0),
    sort_po(Pairs0, Pairs).

%!  sort_po(+Pairs, -Sorted) is det.
%
%   Sort a list of P-ValueList. This is   used  to keep the dominant
%   rdf, rdfs, skos, etc. properties in a   fixed order at the start
%   of the table.

sort_po(Pairs, Sorted) :-
    map_list_to_pairs(po_key, Pairs, Keyed),
    keysort(Keyed, KeySorted),
    exclude(=(0-_), KeySorted, Remaining),
    pairs_values(Remaining, Sorted).

po_key(P-_, Key) :-
    p_order(P, Key),
    !.
po_key(P-_, Key) :-
    label_sort_key(P, Key).

%!  p_order(+P, -SortKey) is semidet.
%
%   SortKey is the key used for sorting the predicate P.
%
%   @tbd    Make this hookable.

:- rdf_meta
    p_order(r,?).

p_order(P, Order) :-
    cliopatria:predicate_order(P, Order),
    !.
p_order(P, 100) :-
    label_property(P),
    !.
p_order(P, 110) :-
    rdfs_subproperty_of(P, skos:altLabel),
    !.
p_order(rdf:type,         210).
p_order(rdfs:subClassOf,  220).
p_order(rdfs:domain,      230).
p_order(rdfs:range,       240).
p_order(rdfs:comment,     310).
p_order(rdfs:isDefinedBy, 320).


%!  uri_info(+URI, +Graph)// is det.
%
%   Display additional info and actions about   a URI in the context
%   of the given graph.

uri_info(URI, Graph) -->
    uri_class_info(URI, Graph),
    uri_predicate_info(URI, Graph),
    html(h2('Context graph')),
    context_graph(URI, []).

uri_class_info(URI, Graph) -->
    { rdf_current_predicate(URI)
    },
    !,
    html(h2('Predicate statistics')),
    predicate_table([URI], Graph, []).
uri_class_info(_,_) --> [].

uri_predicate_info(URI, Graph) -->
    { \+ \+ rdf(_, rdf:type, URI, Graph)
    },
    !,
    html(h2('Class statistics')),
    class_table([URI], Graph, []).
uri_predicate_info(_, _) --> [].


%!  context_graph(+URI, +Options)// is det.
%
%   Show graph with  the  context  of   URI.  Options  is  passed to
%   cliopatria:context_graph/3  and  cliopatria:node_shape/3.    Two
%   options have special meaning:
%
%       * style(?Style)
%       If this option is not specified, it is passed as a variable.
%       It can be tested or filled by cliopatria:context_graph/3 and
%       subsequently used by cliopatria:node_shape/3.
%
%       * start(+URI)
%       Passed to cliopatria:node_shape/3 to indicate the origin of
%       the context graph.

context_graph(URI, Options) -->
    { merge_options(Options, [style(_)], GraphOption),
      rdf_equal(owl:sameAs, SameAs)
    },
    html([ \graphviz_graph(context_graph(URI, GraphOption),
                           [ object_attributes([width('100%')]),
                             wrap_url(resource_link),
                             graph_attributes([ rankdir('RL')
                                              ]),
                             shape_hook(shape(URI, GraphOption)),
                             bag_shape_hook(bag_shape(GraphOption)),
                             edge_hook(edge(URI, GraphOption)),
                             label_hook(cliopatria:node_label),
                             smash([SameAs])
                           ])
         ]).

:- public
    shape/5,
    edge/5,
    bag_shape/3.

%!  shape(+Start, +Options, +URI, -Shape, +GVOptions) is semidet.
%
%   Specify GraphViz shape for URI. This   predicate  calls the hook
%   cliopatria:node_shape/3.

shape(Start, Options, URI, Shape, GVOptions) :-
    append(Options, GVOptions, AllOptions),
    cliopatria:node_shape(URI, Shape, [start(Start)|AllOptions]),
    !.
shape(Start, _Options, Start,
      [ shape(tripleoctagon),style(filled),fillcolor('#ff85fd'),id(start) ],
      _GVOptions).

%!  bag_shape(+Options, +Members, -Shape) is semidet.
%
%   Compute properties for a bag

bag_shape(Options, Members, Shape) :-
    cliopatria:bag_shape(Members, Shape, Options),
    !.
bag_shape(_, _, []).

edge(Start, Options, Predicate, Shape, GVOptions) :-
    append(Options, GVOptions, AllOptions),
    cliopatria:edge_shape(Predicate, Shape, [start(Start)|AllOptions]),
    !.

%!  context_graph(+URI, -Triples, +Options) is det.
%
%   Triples is a graph  that  describes   the  environment  of  URI.
%   Currently, the environment is defined as:
%
%       * skos:related properties (using 1 step)
%       * Transitive properties (using upto 3 steps).
%
%   This predicate can be hooked using cliopatria:context_graph/2.

context_graph(URI, Options, RDF) :-
    cliopatria:context_graph(URI, RDF, Options),
    !.
context_graph(URI, _Options, RDF) :-            % Compatibility
    cliopatria:context_graph(URI, RDF),
    !.
context_graph(URI, _, RDF) :-
    findall(T, context_triple(URI, T), RDF0),
    sort(RDF0, RDF1),
    minimise_graph(RDF1, RDF2),             % remove inverse/symmetric/...
    bagify_graph(RDF2, RDF3, Bags, []),     % Create bags of similar resources
    append(RDF3, Bags, RDF).

:- rdf_meta
    transitive_context(r),
    context(r).

context_triple(URI, Triple) :-
    transitive_context(CP),
    parents(URI, CP, Triples, [URI], 3),
    member(Triple, Triples).
context_triple(URI, Triple) :-
    cliopatria:context_predicate(URI, R),
    rdf_has(URI, R, O, P),
    normalize_triple(rdf(URI, P, O), Triple).
context_triple(URI, Triple) :-
    context(R),
    rdf_has(URI, R, O, P),
    normalize_triple(rdf(URI, P, O), Triple).
context_triple(URI, Triple) :-
    context(R),
    rdf_has(S, R, URI, P),
    normalize_triple(rdf(S, P, URI), Triple).

normalize_triple(rdf(S, inverse_of(P0), O),
                 rdf(O, P, S)) :-
    !,
    rdf_predicate_property(P0, inverse_of(P)).
normalize_triple(RDF, RDF).



parents(URI, Up, [Triple|T], Visited, MaxD) :-
    succ(MaxD2, MaxD),
    rdf_has(URI, Up, Parent, P),
    normalize_triple(rdf(URI, P, Parent), Triple),
    \+ memberchk(Parent, Visited),
    parents(Parent, Up, T, [Parent|Visited], MaxD2).
parents(_, _, [], _, _).

transitive_context(owl:sameAs).
transitive_context(rdfs:subClassOf).
transitive_context(rdfs:subPropertyOf).
transitive_context(skos:broader).
transitive_context(P) :-
    rdfs_individual_of(P, owl:'TransitiveProperty'),
    rdf_predicate_property(P, rdfs_subject_branch_factor(BF)),
    BF < 2.0.

context(skos:related).
context(skos:mappingRelation).

%!  list_triples(+Request)
%
%   List  triples  for  a  given    predicate.  The  triple-set  can
%   optionally be filtered on the graph, type of the subject or type
%   of the object.

list_triples(Request) :-
    http_parameters(Request,
                    [ predicate(P,
                                [ optional(true),
                                  description('Limit triples to this pred')]),
                      graph(Graph, [ optional(true),
                                     description('Limit triples to this graph')
                                   ]),
                      domain(Dom,  [ optional(true),
                                     description('Restrict to subjects of this class')
                                   ]),
                      range(Range, [ optional(true),
                                     description('Restrict to objects of this class')
                                   ])
                    ]),
    authorized_browse(Graph),
    (   atom(Dom)
    ->  findall(rdf(S,P,O), rdf_in_domain(S,P,O,Dom,Graph), Triples0)
    ;   atom(Range)
    ->  findall(rdf(S,P,O), rdf_in_range(S,P,O,Range,Graph), Triples0)
    ;   findall(rdf(S,P,O), rdf(S,P,O,Graph), Triples0)
    ),
    sort(Triples0, Triples),
    sort_triples_by_label(Triples, Sorted),
    length(Sorted, Count),
    (   var(P)
    ->  Title = 'Triples in graph ~w'-[Graph]
    ;   rdf_display_label(P, PLabel),
        Title = 'Triples for ~w in graph ~w'-[PLabel, Graph]
    ),
    reply_html_page(cliopatria(default),
                    title(Title),
                    [ h1(\triple_header(Count, P, Dom, Range, Graph)),
                      \triple_table(Sorted, P, [resource_format(nslabel)])
                    ]).

rdf_in_domain(S,P,O,Dom,Graph) :-
    rdf(S, P, O, Graph),
    rdf_has(S, rdf:type, Dom).

rdf_in_range(S,P,O,Lit,Graph) :-
    rdf_equal(rdfs:'Literal', Lit),
    !,
    O = literal(_),
    rdf(S, P, O, Graph).
rdf_in_range(S,P,O,Rng,Graph) :-
    rdf_equal(rdfs:'Resource', Rng),
    !,
    rdf(S, P, O, Graph),
    atom(O).
rdf_in_range(S,P,O,Rng,Graph) :-
    rdf(S, P, O, Graph),
    rdf_has(O, rdf:type, Rng).


triple_header(Count, Pred, Dom, Range, Graph) -->
    html([ 'Table for the ~D triples'-[Count],
           \for_predicate(Pred),
           \with_domain(Dom),
           \with_range(Range),
           \in_graph([graph(Graph)])
         ]).

with_domain(Dom) -->
    { var(Dom) },
    !.
with_domain(Dom) -->
    html([' with domain ', \rdf_link(Dom, [role(domain)])]).

with_range(Range) -->
    { var(Range) },
    !.
with_range(Range) -->
    html([' with range ', \rdf_link(Range, [role(range)])]).

%!  triple_table(+Triples, +Predicate, +Options)// is det.
%
%   Show a list  of  triples.  If   Predicate  is  given,  omit  the
%   predicate from the table.

triple_table(Triples, Pred, Options) -->
    { option(top_max(TopMax), Options, 500),
      option(top_max(BottomMax), Options, 500)
    },
    html(table(class(block),
               [ \spo_header(Pred)
               | \table_rows_top_bottom(spo_row(Options, Pred), Triples,
                                        TopMax, BottomMax)
               ])).

spo_header(P) -->
    { nonvar(P) },
    html(tr([ th('Subject'),
              th('Object')
            ])).
spo_header(_) -->
    html(tr([ th('Subject'),
              th('Predicate'),
              th('Object')
            ])).

spo_row(Options, Pred, rdf(S,_,O)) -->
    { nonvar(Pred) },
    !,
    html([ td(class(subject), \rdf_link(S, [role(subj)|Options])),
           td(class(object),  \rdf_link(O, [role(obj) |Options]))
         ]).
spo_row(Options, _, rdf(S,P,O)) -->
    html([ td(class(subject),   \rdf_link(S, [role(subj)|Options])),
           td(class(predicate), \rdf_link(P, [role(pred)|Options])),
           td(class(object),    \rdf_link(O, [role(obj) |Options]))
         ]).


%!  list_triples_with_object(+Request)
%
%   HTTP handler that creates a  subject/predicate table for triples
%   that have the gived _object_. Object   is specified using either
%   the =r= or =l= parameter. Optionally,  results can be limited to
%   a predicate and/or graph.

list_triples_with_object(Request) :-
    http_parameters(Request,
                    [ r(RObject,   [optional(true),
                                    description('Object as resource (URI)')
                                   ]),
                      l(LObject,   [optional(true),
                                    description('Object as literal (Prolog notation)')
                                   ]),
                      p(P,         [optional(true),
                                    description('Limit to a given predicate (URI)')
                                   ]),
                      graph(Graph, [optional(true),
                                    description('Limit to a given graph (URI)')
                                   ]),
                      sortBy(Sort,
                             [ oneof([label, subject, predicate]),
                               default(label),
                               description('How to sort the result')
                             ])
                    ]),
    authorized_browse(Graph),
    target_object(RObject, LObject, Object),
    include(ground, [graph(Graph), predicate(P)], Options),
    list_triples_with_object(Object, [sortBy(Sort)|Options]).

target_object(RObject, _LObject, RObject) :-
    atom(RObject),
    !.
target_object(_, LObject, Object) :-
    atom(LObject),
    !,
    term_to_atom(Object0, LObject),
    rdf11_rdf_db(Object0, Object).
target_object(_, _, _) :-
    throw(existence_error(http_parameter, r)).

rdf11_rdf_db(^^(String, Type), literal(type(Type, Atom))) :-
    atom_string(Atom, String).
rdf11_rdf_db(@(String, Lang), literal(lang(Lang, Atom))) :-
    atom_string(Atom, String).
rdf11_rdf_db(literal(Lit),   literal(Lit)).


%!  list_triples_with_literal(+Request)
%
%   List triples that have a literal   that matches the q-parameter.
%   This is used for  finding   objects  through  the autocompletion
%   interface.

list_triples_with_literal(Request) :-
    authorized_browse(default),
    http_parameters(Request,
                    [ q(Text,
                        [optional(true),
                         description('Object as resource (URI)')
                        ])
                    ]),
    list_triples_with_object(literal(Text), [sortBy(subject)]).


list_triples_with_object(Object, Options) :-
    option(graph(Graph), Options, _),
    option(predicate(P), Options, _),
    findall(S-P, rdf(S,P,Object,Graph), Pairs),
    (   option(sortBy(label), Options)
    ->  sort_pairs_by_label(Pairs, Sorted)
    ;   option(sortBy(predicate), Options)
    ->  transpose_pairs(Pairs, Transposed), % flip pairs and sort on new key
        flip_pairs(Transposed, Sorted)      % flip back without sort
    ;   sort(Pairs, Sorted)
    ),
    length(Pairs, Count),
    label_of(Object, OLabel),
    reply_html_page(cliopatria(default),
                    title('Triples with object ~w'-[OLabel]),
                    [ h1(\otriple_header(Count, Object, Options)),
                      \otriple_table(Sorted, Object, [resource_format(nslabel)])
                    ]).

otriple_header(Count, Object, Options) -->
    html([ 'Table for the ~D triples'-[Count],
           \with_object(Object),
           \on_predicate(Options),
           \in_graph(Options),
           \sortBy(Options)
         ]).

with_object(Obj) -->
    { var(Obj)},
    !.
with_object(Obj) -->
    html([' with object ', \rdf_link(Obj, [role(obj)])]).

on_predicate(Options) -->
    { option(predicate(P), Options),
      nonvar(P)
    },
    !,
    html([' on predicate ', \rdf_link(P, [role(pred)])]).
on_predicate(_) -->
    [].

otriple_table(SPList, Object, Options) -->
    { option(top_max(TopMax), Options, 500),
      option(top_max(BottomMax), Options, 500)
    },
    html(table(class(block),
               [ \sp_header(Object)
               | \table_rows_top_bottom(sp_row(Options,Object), SPList,
                                        TopMax, BottomMax)
               ])).

sp_header(_) -->
    html(tr([ th('Subject'),
              th('Predicate')
            ])).

sp_row(Options, _O, S-P) -->
    html([ td(class(subject),   \rdf_link(S, [role(subj)|Options])),
           td(class(predicate), \rdf_link(P, [role(pred)|Options]))
         ]).





                 /*******************************
                 *            RDF UTIL          *
                 *******************************/

%!  sort_by_label(+URIs, -Sorted) is det.
%
%   Sort a list of URIs by their label using locale-based ordering.

sort_by_label(URIs, Sorted) :-
    map_list_to_pairs(label_sort_key, URIs, LabelPairs),
    keysort(LabelPairs, SortedPairs),
    pairs_values(SortedPairs, Sorted).

label_sort_key(URI, Key) :-
    label_of(URI, Label),
    (   atom(Label)
    ->  collation_key(Label, Key)
    ;   Key = Label
    ).

label_of(URI, Label) :-
    rdf_is_resource(URI),
    !,
    rdf_display_label(URI, Label).
label_of(Literal, Label) :-
    literal_text(Literal, Label).


%!  sort_triples_by_label(+Triples, -Sorted)
%
%   Sort a list of rdf(S,P,O) by the labels.

sort_triples_by_label(Pairs, Sorted) :-
    map_list_to_pairs(key_triple_by_label, Pairs, LabelPairs),
    keysort(LabelPairs, SortedPairs),
    pairs_values(SortedPairs, Sorted).

key_triple_by_label(rdf(S,P,O), rdf(SK,PK,OK)) :-
    label_sort_key(S, SK),
    label_sort_key(P, PK),
    label_sort_key(O, OK).

%!  sort_pairs_by_label(+Pairs, -Sorted)
%
%   Sort a pair-list where the keys are resources by their label.

sort_pairs_by_label(Pairs, Sorted) :-
    map_list_to_pairs(key_label_sort_key, Pairs, LabelPairs),
    keysort(LabelPairs, SortedPairs),
    pairs_values(SortedPairs, Sorted).

key_label_sort_key(R-_, Key) :-
    label_sort_key(R, Key).


                 /*******************************
                 *        CUSTOMIZATION         *
                 *******************************/

%!  p_label(+Id, -Label)
%
%   Defines the visible label for a property.
%
%   @see    html_property_table//2.

p_label(source(_), 'Source URL').
p_label(triples(G),
        ['# ', a(href(Link), triples)]) :-
    http_link_to_id(list_triples, [graph=G], Link).
p_label(subject_count(G),
        ['# ', a(href(Link), subjects)]) :-
    http_link_to_id(list_instances, [graph=G], Link).
p_label(bnode_count(G),
        ['# ', a(href(Link), 'bnode subjects')]) :-
    http_link_to_id(list_instances, [graph=G, type=bnode], Link).
p_label(predicate_count(G),
        ['# ', a(href(Link), predicates)]) :-
    http_link_to_id(list_predicates, [graph=G], Link).
p_label(type_count(G),
        ['# Referenced ', a(href(Link), classes)]) :-
    http_link_to_id(list_classes, [graph=G], Link).


                 /*******************************
                 *            SEARCH            *
                 *******************************/

%!  search(+Request)
%
%   HTTP handler to search for triples   that contain a literal that
%   matches a query.
%
%   @tbd    Produce a sensible search language.

search(Request) :-
    authorized_browse(default),
    http_parameters(Request,
                    [ q(QueryText,
                        [ description('Query to search for')
                        ]),
                      filter(FilterAtom,
                             [ optional(true),
                               description('Filter on raw matches (a Prolog term)')
                             ])
                    ]),
    (   var(FilterAtom)
    ->  Filter = true
    ;   atom_to_term(FilterAtom, Filter0, []),
        rdf_global_term(Filter0, Filter)
    ),

    find_literals(QueryText, Literals, Query),
    literal_triples(Literals, Filter, Triples),
    reply_html_page(cliopatria(default),
                    title('Search results for ~q'-[Query]),
                    [ h1('Search results for token "~q"'-[Query]),
                      \rdf_table(Triples, [])
                    ]).

find_literals(QueryText, [Query], exact(Query)) :-
    % Check if Q starts and ends with double quotes:
    sub_atom(QueryText,0,1,Remainder,'"'),
    sub_atom(QueryText,Remainder,1,0,'"'),
    !,
    sub_atom(QueryText,1,_,1,Query).
find_literals(QueryText, Literals, Query) :-
    % if not quoted, perform search on tokenized query
    tokenize_atom(QueryText, Tokens),
    once(phrase(query(Query), Tokens)),
    rdf_find_literals(Query, Literals).

query(Query) -->
    simple_query(Q1),
    (   eos
    ->  {Query = Q1}
    ;   query(Q2),
        {Query = and(Q1,Q2)}
    ).

eos([],[]).

simple_query(Token) -->
    ['"',Token,'"'],
    !.
simple_query(not(Token)) -->
    [-, Token].
simple_query(case(Token)) -->
    [Token].

%!  literal_triples(+ListOfLiterals, +Filter, -Triples) is det.
%
%   Find the list of triples with   a  literal in ListOfLiterals and
%   whose subject satisfies Filter.

literal_triples(Literals, Filter, Triples) :-
    sub_term(graph(Graph), Filter),
    !,
    phrase(ltriples(Literals, Graph, Filter), Triples).
literal_triples(Literals, Filter, Triples) :-
    phrase(ltriples(Literals, Filter), Triples).


ltriples([], _, _) --> [].
ltriples([H|T], G, F) -->
    findall(rdf(S,P,literal(L)),
            (   rdf(S,P,literal(exact(H), L),G),
                search_filter(F, S)
            )),
    ltriples(T, G, F).

ltriples([], _) --> [].
ltriples([H|T], F) -->
    findall(rdf(S,P,literal(L)),
            (   rdf(S,P,literal(exact(H), L)),
                search_filter(F, S)
            )),
    ltriples(T, F).

%!  rdf_table(+Triples, +Options)// is det.
%
%   Emit a table of triples.
%
%   @param Triples is a list of rdf(S,P,O).

rdf_table(Triples, Options) -->
    { option(top_max(TopMax), Options, 500),
      option(top_max(BottomMax), Options, 500)
    },
    html(table(class(block),
               [ tr([ th('Subject'), th('Predicate'), th('Object') ])
               | \table_rows_top_bottom(triple, Triples,
                                        TopMax, BottomMax)
               ])).

triple(rdf(S,P,O)) -->
    html([ td(class(subject),   \rdf_link(S, [role(subj)])),
           td(class(predicate), \rdf_link(P, [role(pred)])),
           td(class(object),    \rdf_link(O, [role(obj) ]))
         ]).


                 /*******************************
                 *     HTML INFRASTRUCTURE      *
                 *******************************/

%!  html_property_table(+Template, :Goal)// is det.
%
%   Create a table for all instantiations of Template for which Goal
%   is true. Template is a term row(C1,   C2, ...). The first column
%   (C1) is considered the property-name and   emitted  as a cell of
%   class =p_name=. The label for  the   property  is  derived using
%   p_label/2. The remainder is emited as normal td value-cells.

html_property_table(Template, Goal) -->
    { findall(Template, Goal, Rows) },
    html(table(class(block),
               \table_rows(prow, Rows))).

prow(Row) -->
    { Row =.. [_,H|Cells],
      (   p_label(H, Label0)
      ->  true
      ;   functor(H, Label0, _)
      ),
      (   is_list(Label0)
      ->  append(Label0, [:], Label)
      ;   Label = [Label0, :]
      )
    },
    html([ th(class(p_name), Label)
         | \pcells(Cells)
         ]).

pcells([]) --> [].
pcells([H|T]) -->
    pcell(H),
    pcells(T).

pcell(int(Value)) -->
    { integer(Value) },
    !,
    nc('~D', Value).
pcell(H) -->
    { compound(H),
      H =.. [Class,Value], !
    },
    html(td(class(Class), Value)).
pcell(H) -->
    html(td(H)).


%!  table_rows(:Goal, +DataList)// is det.
%!  table_rows(:Goal, +DataList, +MaxTop, +MaxBottom)// is det.
%
%   Emit a number of table rows (=tr=).   The content of each row is
%   created by calling call(Goal, Data)  as   a  DCG.  The rows have
%   alternating classes =even= and =odd=.  The first row is =odd=.
%
%   The variation table_rows//4  limits  the   size  of  the  table,
%   placing a cell with  class  =skip=,   indicating  the  number of
%   skipped rows.
%
%   Note that we can also achieve  alternate colouring using the CSS
%   pseudo classes =|tr:nth-child(odd)|= and =|tr:nth-child(even)|=.

table_rows(Goal, Rows) -->
    table_rows(Rows, Goal, 1, -1).

table_rows_top_bottom(Goal, Rows, inf, inf) -->
    !,
    table_rows(Rows, Goal, 1, -1).
table_rows_top_bottom(Goal, Rows, MaxTop, MaxBottom) -->
    { length(Rows, Count) },
    (   { MaxTop+MaxBottom >= Count }
    ->  table_rows(Rows, Goal, 1, -1)
    ;   { Skip is Count-MaxBottom,
          delete_list_prefix(Skip, Rows, BottomRows),
          Skipped is Count-(MaxTop+MaxBottom)
        },
        table_rows(Rows, Goal, 1, MaxTop),
        html(tr(class(skip),
                [ th(colspan(10), 'Skipped ~D rows'-[Skipped])
                ])),
        table_rows(BottomRows, Goal, 1, -1)
    ).

table_rows(_, _, _, 0) --> !, [].
table_rows([], _, _, _) --> [].
table_rows([H|T], Goal, N, Left) -->
    { N2 is N + 1,
      (   N mod 2 =:= 0
      ->  Class = even
      ;   Class = odd
      ),
      Left2 is Left - 1
    },
    html(tr(class(Class), \call(Goal, H))),
    table_rows(T, Goal, N2, Left2).

delete_list_prefix(0, List, List) :- !.
delete_list_prefix(_, [], []) :- !.
delete_list_prefix(N, [_|T], List) :-
    N2 is N - 1,
    delete_list_prefix(N2, T, List).

%!  list_prefixes(+Request)
%
%   List known RDF prefixes in various formats

list_prefixes(Request) :-
    authorized_browse(default),
    Formats = [html,turtle],
    http_parameters(Request,
                    [ format(Format,
                             [ oneof(Formats),
                               description('Output format'),
                               default(html)
                             ])
                    ]),
    findall(Prefix-URI,
            rdf_current_ns(Prefix, URI),
            Pairs),
    keysort(Pairs, Sorted),
    prefix_actions(Options),
    reply_html_page(cliopatria(default),
                    title('RDF prefixes (namespaces)'),
                    [ h1('Known RDF prefixes (namespaces)'),
                      \explain_prefixes,
                      \prefix_table(Format, Sorted, Options),
                      \prefix_formats(Formats, Format, Request)
                    ]).

prefix_actions([edit(true)]) :-
    logged_on(User),
    !,
    catch(check_permission(User, write(_, del_prefix(_))), _, fail),
    !.
prefix_actions([]).

explain_prefixes -->
    html(p([ 'The following prefixes are known and may be used \c
                  without declaration in SPARQL queries to this server.'
           ])).

prefix_formats(Formats, Format, Request) -->
    { select(Format, Formats, Alt)
    },
    html(p(class('prefix-format'),
           [ 'Also available in ',
             \alt_formats(Alt, Request)
           ])).

alt_formats([], _) --> [].
alt_formats([H|T], Request) -->
    { http_reload_with_parameters(Request, [format(H)], HREF)
    },
    html(a(href(HREF), H)),
    (   {T==[]}
    ->  []
    ;   html(' and '),
        alt_formats(T, Request)
    ).

prefix_table(html, Pairs, Options) -->
    html(table(class(block),
               [ \prefix_table_header,
                 \table_rows(prefix_row(Options), Pairs)
               ])).
prefix_table(turtle, Pairs, _) -->
    html(pre(class(code),
             \turtle_prefixes(Pairs))).

prefix_table_header -->
    html(tr([ th('Prefix'),
              th('URI')
            ])).

prefix_row(Options, Prefix-URI) -->
    { option(edit(true), Options),
      !,
      http_link_to_id(del_prefix, [prefix(Prefix)], HREF)
    },
    html([ td(Prefix),
           td(URI),
           td(a([ href(HREF),
                  class('delete'),
                  title('Remove prefix')
                ], '\u232B'))
         ]).
prefix_row(_Options, Prefix-URI) -->
    html([ td(Prefix),
           td(URI)
         ]).

turtle_prefixes(Pairs) -->
    { longest_prefix(Pairs, 0, Length),
      PrefixCol is Length+10
    },
    turtle_prefixes(Pairs, PrefixCol).

longest_prefix([], L, L).
longest_prefix([Prefix-_|T], L0, L) :-
    atom_length(Prefix, L1),
    L2 is max(L0, L1),
    longest_prefix(T, L2, L).

turtle_prefixes([], _) --> [].
turtle_prefixes([Prefix-URI|T], Col) -->
    html('@prefix ~t~w: ~*|<~w> .~n'-[Prefix, Col, URI]),
    turtle_prefixes(T, Col).

authorized_browse(Graph), var(Graph) =>
    authorized(read(default, browse)).
authorized_browse(Graph) =>
    authorized(read(Graph, browse)).
