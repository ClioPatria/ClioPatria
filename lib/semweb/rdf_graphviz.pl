/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam,
                              VU University Amsterdam,
                              CWI, Amsterdam
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

:- module(rdf_graphviz,
          [ gviz_write_rdf/3            % +Stream, +Triples, +Options
          ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_write)).
:- use_module(library(http/url_cache)).
:- use_module(library(assoc)).
:- use_module(library(option)).
:- use_module(library(gensym)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(ugraphs)).
:- use_module(library(semweb/rdf_label)).

:- rdf_register_ns(graphviz, 'http://www.graphviz.org/').

/** <module> Interface to graphviz for RDF graphs

Graphviz is a general purpose graph vizualization library. Its home-page
is  http://www.graphviz.org/  This  module  translates    an  RDF  graph
represented as a list of rdf(S,P,O) into a .dot file.

@author Jan Wielemaker
*/

%!  gviz_write_rdf(+Stream, +Graph, +Options) is det.
%
%   Write the graph Triples to Stream in =dot= compatible format.
%   Options:
%
%       * graph_attributes(+Attributes)
%       Additional overall graphs attributes for dot.  Each
%       attribute is of the format Name(Value) and written
%       as Name="Value".  The term size(W,H) is accepted as
%       well.
%
%       * max_label_length(+Len)
%       Truncate labels to Len characters.  Default is 25.
%       Use =inf= to print the full label.
%
%       * lang(+Lang)
%       Lang is the language used for the labels.  See
%       resource_label/4.
%
%       * smash(+Properties)
%       Smash networks connected by one of the given properties.
%       Currently only [owl:sameAs].
%
%       * bags(Bags)
%       How to handle bags.  Values are
%
%           * graph
%           Show as normal nodes (i.e. handles as normal RDF)
%
%           * merge(Shape, Max)
%           Put the members in a dot node using Shape.  Only
%           place the first Max members and add a note stating
%           '... showing N of M' to indicate the actual number
%
%       * edge_links(+Boolean)
%       If =false= (default =true=) do not put href atributes on
%       edges.
%
%       * wrap_url(:Goal)
%       If present, URLs of the graph are replaced with the
%       result of call(Goal, URL0, URL)
%
%       * edge_hook(:Goal)
%       Called to define the attributes for a link as
%       call(Goal, URI, Attributes, Options). Attributes is a list
%       of Name(Value) terms. See edge_attributes/3.
%
%       * shape_hook(:Goal)
%       Called to define the shape of a resource as call(Goal, URI,
%       Shape, Options).  Shape is a list of Name(Value) terms.  See
%       shape/3.
%
%       * bag_shape_hook(:Goal)
%       Called to define the shape parameters for a bag (Table).
%       called as call(Goal, Members, Shape) Shape is a list of
%       Name(Value) terms.
%
%       * label_hook(:Goal)
%       Called to define the label of a resource as call(Goal, URI,
%       Language, MaxLength, Label). Label is an atom.
%
%       * target(Target)
%       If present, add target=Target to all attribute lists that
%       have an =href= attribute.
%
%       * display_lang(+Boolean)
%       Display the language of literal nodes, defaults to true.
%

:- meta_predicate gviz_write_rdf(+,+,:).
:- rdf_meta gviz_write_rdf(+,t,t).

gviz_write_rdf(Stream, Graph0, Options0) :-
    exclude(exclude_triple, Graph0, Graph1),
    meta_options(is_meta, Options0, Options),
    debug(edge(hook), 'Options = ~p', [Options]),
    format(Stream, 'digraph G~n{ ', []),
    option(graph_attributes(Attrs), Options, []),
    write_graph_attributes(Attrs, Stream),
    smash_graph(Graph1, Graph2, Options),
    combine_bags(Graph2, Triples, Bags, Options),
    gv_write_edges(Triples, Done, Stream,
                   [ graph(Graph0)
                   | Options
                   ]),
    assoc_to_list(Done, Nodes),
    gv_write_nodes(Nodes, Stream,
                   [ bag_assoc(Bags),
                     graph(Graph0)
                   | Options
                   ]),
    format(Stream, '~n}~n', []).

is_meta(wrap_url).
is_meta(shape_hook).
is_meta(edge_hook).
is_meta(bag_shape_hook).
is_meta(label_hook).

%!  write_graph_attributes(+List, +Out)
%
%   Write attributes for the graph as a whole

write_graph_attributes([], _).
write_graph_attributes([H|T], Out) :-
    write_graph_attribute(H, Out),
    write_graph_attributes(T, Out).

write_graph_attribute(size(W,H), Out) :-
    !,
    format(Out, '  size="~w,~w";~n', [W, H]).
write_graph_attribute(AttVal, Out) :-
    AttVal =.. [Name, Value],
    format(Out, '  ~w="~w";~n', [Name, Value]).


%!  combine_bags(+Graph, -Triples, -Bags, +Options) is det.
%
%   Seperate  the  bags  from  the   graph.  Triples  represent  the
%   remaining graph (in which a bag is a single node) and Bags is an
%   assoc BagID-Members storing the members of the bags.

combine_bags(Graph, Graph, Bags, Options) :-
    option(bags(graph), Options),
    !,
    empty_assoc(Bags).
combine_bags(Graph, Triples, Bags, _Options) :-
    empty_assoc(Bags0),
    find_bags(Graph, Graph1, Bags0, Bags1),
    collect_bags(Graph1, Triples, Bags1, Bags).

:- rdf_meta find_bags(t, -, +, -).

find_bags([], [], Bags, Bags).
find_bags([rdf(S,rdf:type,rdf:'Bag')|Graph], Triples, Bags0, Bags) :-
    !,
    put_assoc(S, Bags0, [], Bags1),
    find_bags(Graph, Triples, Bags1, Bags).
find_bags([H|T0], [H|T], Bags0, Bags) :-
    find_bags(T0, T, Bags0, Bags).

collect_bags([], [], Bags, Bags).
collect_bags([rdf(S,P,O)|Graph], Triples, Bags0, Bags) :-
    bagid_property(P, _),
    get_assoc(S, Bags0, L, Bags1, [O|L]),
    !,
    collect_bags(Graph, Triples, Bags1, Bags).
collect_bags([H|T0], [H|T], Bags0, Bags) :-
    collect_bags(T0, T, Bags0, Bags).


%!  bagid_property(+P, -I) is semidet.
%
%   True if P is of the format   =|_:N|=,  where N is a non-negative
%   integer.

bagid_property(P, I) :-
    atom(P),
    !,
    string_concat('_:', N, P),
    number_string(I, N),
    integer(I), I >= 0.
bagid_property(P, I) :-
    atom_concat('_:', I, P).

%!  smash_graph(+GraphIn, -GraphOut, +Options)
%
%   Smash networks of equivalent properties.

smash_graph(GraphIn, GraphOut, Options) :-
    option(smash(Props), Options, []),
    !,
    smash_graph_(Props, GraphIn, GraphOut).

smash_graph_([], Graph, Graph).
smash_graph_([H|T], Graph0, Graph) :-
    smash_on_property(H, Graph0, Graph1),
    smash_graph_(T, Graph1, Graph).

%!  smash_on_property(+P, +GraphIn, -GraphOut)
%
%   Merge owl:sameAs nodes, replacing the node with a bag.

smash_on_property(P, GraphIn, GraphOut) :-
    smash_edges(GraphIn, P, Edges, Rest),
    vertices_edges_to_ugraph([], Edges, Graph),
    partition_ugraph(Graph, VerticeSets),
    make_eq_bags(VerticeSets, VerticeBags, MapAssoc),
    maplist(smash_triple(MapAssoc), Rest, Mapped),
    append(Mapped, VerticeBags, GraphOut).

smash_edges([], _, [], []).
smash_edges([rdf(S,P,O)|T0], P, [S-O,O-S|T], Rest) :-
    !,
    smash_edges(T0, P, T, Rest).
smash_edges([H|T0], P, Edges, [H|T]) :-
    smash_edges(T0, P, Edges, T).

partition_ugraph([], []) :- !.
partition_ugraph(G0, [Vs0|Vs]) :-
    G0 = [V-_|_],
    reachable(V, G0, Vs0),
    del_vertices(G0, Vs0, G1),
    partition_ugraph(G1, Vs).

make_eq_bags(Vertices, Bags, MapAssoc) :-
    make_eq_bags(Vertices, 1, Bags, Mapping),
    list_to_assoc(Mapping, MapAssoc).

:- rdf_meta make_eq_bags(+, +, t, -).

make_eq_bags([], _, [], []).
make_eq_bags([Vs|T0], I, [rdf(BagId, rdf:type, rdf:'Bag')|Bags], Mapping) :-
    atom_concat('_:sameAs', I, BagId),
    make_eq_bag(Vs, 1, BagId, Bags, BagsT),
    make_mapping(Vs, BagId, Mapping, MappingT),
    I2 is I + 1,
    make_eq_bags(T0, I2, BagsT, MappingT).

make_eq_bag([], _, _, Triples, Triples).
make_eq_bag([H|T], I, BagId, [rdf(BagId, P, H)|Triples0], Triples) :-
    bagid_property(P, I),
    I2 is I + 1,
    make_eq_bag(T, I2, BagId, Triples0, Triples).

make_mapping([], _, Mapping, Mapping).
make_mapping([H|T], BagId, [H-BagId|Mapping0], Mapping) :-
    make_mapping(T, BagId, Mapping0, Mapping).

smash_triple(Mapping, rdf(S0,P,O0), rdf(S,P,O)) :-
    smash(Mapping, S0, S),
    smash(Mapping, O0, O).

smash(Assoc, R0, R) :-
    get_assoc(R0, Assoc, R),
    !.
smash(_, R, R).


%!  gv_write_edges(+Graph, -Done, +Stream, +Options) is det.
%
%   Write the edges of an RDF graph   in  =dot= format. It invents a
%   dot identifier for each node  as   it  processes  the nodes. The
%   mapping from node to dot  identifier   is  returned in the assoc
%   Done.

gv_write_edges(Graph, Done, Stream, Options) :-
    empty_assoc(Done0),
    gv_write_edges(Graph, Done0, Done, Stream, Options).

gv_write_edges([], Done, Done, _, _).
gv_write_edges([Triple|T], Done0, Done, Stream, Options) :-
    write_edge(Triple, Done0, Done1, Stream, Options),
    gv_write_edges(T, Done1, Done, Stream, Options).

write_edge(rdf(S,P,O), Done0, Done2, Stream, Options) :-
    format(Stream, '  ', []),
    write_node_id(S, Done0, Done1, Stream),
    write(Stream, ' -> '),
    write_node_id(O, Done1, Done2, Stream),
    edge_attributes(rdf(S,P,O), Attributes0, Options),
    (   option(label(Label), Attributes0)
    ->  Attributes = Attributes0
    ;   resource_label(P, Label, Options),
        Attributes = [label(Label)|Attributes0]
    ),
    (   option(edge_links(true), Options, true)
    ->  wrap_url(P, URL, Options),
        target_option([href(URL), label(Label)|Attributes], Attrs, Options),
        write_attributes(Attrs, Stream)
    ;   write_attributes(Attributes, Stream)
    ),
    nl(Stream).

write_node_id(S, Done, Done, Stream) :-
    get_assoc(S, Done, Id),
    !,
    write(Stream, Id).
write_node_id(S, Done0, Done, Stream) :-
    gensym(n, Id),
    put_assoc(S, Done0, Id, Done),
    write(Stream, Id).

%!  gv_write_nodes(+Nodes:list(pair), +Stream, +Options)
%
%   Write information about the nodes, defining  the share and label
%   of the node.

gv_write_nodes([], _, _).
gv_write_nodes([RDF-ID|T], Stream, Options) :-
    format(Stream, '~w ', [ID]),
    write_node_attributes(RDF, Stream, Options),
    write(Stream, ';\n  '),
    gv_write_nodes(T, Stream, Options).

%!  write_node_attributes(+RDF, +Stream, +Options) is det.
%
%   Write attributes for an RDF node.   The node identifier matching
%   the declared edges is alreadu written to Stream.

write_node_attributes(R, Stream, Options) :-
    rdf_is_resource(R),
    option(bag_assoc(Bags), Options),
    get_assoc(R, Bags, Members),
    !,
    Members = [First|_],
    shape(First, MemberShape0, Options),
    bag_shape(Members, BagShape0, Options),
    exclude(no_bag_option, MemberShape0, MemberShape),
    option(bags(merge(BagShape1, Max0)), Options,
           merge([ shape(box),
                   style('rounded,filled,bold'),
                   fillcolor('#ffff80')
                 ], 5)),
    select_option(max(Max), BagShape0, BagShape2, Max0),
    partition(label_option, BagShape2, LabelOptions0, BagShape2a),
    merge_options(BagShape1, MemberShape, BagShape3),
    merge_options(BagShape2a, BagShape3, BagShape),
    merge_options(LabelOptions0, Options, LabelOptions),
    bag_label(Members, Max, Label, LabelOptions),
    write_attributes([html(Label)|BagShape], Stream).
write_node_attributes(R, Stream, Options) :-
    rdf_is_resource(R),
    !,
    shape(R, Shape, Options),
    wrap_url(R, URL, Options),
    resource_label(R, Label, Options),
    target_option([href(URL), label(Label)|Shape], Attrs, Options),
    (   select(img(IMGOptions), Attrs, RAttrs),
        catch(write_image_node(IMGOptions, RAttrs, Stream, Options),
              error(existence_error(url,URL2),Context),
              ( print_message(warning,
                              error(existence_error(url,URL2),Context)),
                fail))
    ->  true
    ;   delete(Attrs, img(_), RAttrs),
        write_attributes(RAttrs, Stream)
    ).
write_node_attributes(Lit, Stream, Options) :-
    shape(Lit, Shape, Options),
    option(max_label_length(MaxLen), Options, 25),
    literal_text(Lit, Text),
    truncate_atom(Text, MaxLen, Summary0),
    (   ( option(display_lang(true), Options, true),
              Lit = literal(lang(Lang, _)))
    ->  atomic_list_concat([Summary0, '@', Lang], Summary)
    ;   Summary = Summary0
    ),
    write_attributes([label(Summary)|Shape], Stream).

target_option(Attrs0, Attrs, Options) :-
    option(target(Target), Options),
    !,
    Attrs = [target(Target)|Attrs0].
target_option(Attrs, Attrs, _).

no_bag_option(img(_)).
no_bag_option(width(_)).
no_bag_option(height(_)).
no_bag_option(cellpadding(_)).
no_bag_option(fixedsize(_)).
no_bag_option(label(_)).
no_bag_option(border(_)).

label_option(max_label_length(_)).

%!  bag_label(+Members, +Max, -Label, +Options) is det.
%
%   Create an HTML description for describing a bag of objects.
%
%   @param Max is the maximum # members to show.  If there are more,
%          a text "... showing N of M" is displayed.
%   @param Label is a Prolog packed string with HTML text.

bag_label(Members, Max, Label, Options) :-
    length(Members, Len),
    phrase(html(table([ border(0) ],
                      \html_bag_label(Members, 1, Max, Len, Options))),
           Tokens),
    with_output_to(string(Label), print_html(Tokens)).

html_bag_label([], _, _, _, _) --> !.
html_bag_label(_, I, Max, Len, _Options) -->
    { I > Max },
    !,
    html(tr(td([align(right), cellpadding(5)],
               font(face('Helvetica:style=Italic'), '... showing ~D of ~D'-[Max, Len])))).
html_bag_label([H|T], I, Max, Len, Options) -->
    { (   atom(H)
      ->  wrap_url(H, URL, Options),
          target_option([href(URL)], Atts, Options)
      ;   Atts=[]
      )
    },
    html(tr(td([align(left)|Atts], \html_resource_label(H, Options)))),
    { I2 is I + 1 },
    html_bag_label(T, I2, Max, Len, Options).

html_resource_label(Resource, Options) -->
    { resource_label(Resource, Label, Options)
    },
    html(Label).

%!  write_image_node(+ImgAttrs, +Attrs, +Stream, +Options) is det.
%
%   Render a node using an image. The   image  location is either an
%   external URL or a local file   specification  using the notation
%   icons(File), a term that must  resolve   in  an image file using
%   absolute_file_name/3. In the default setup,  this means that the
%   image must be in the directory =|web/icons|= of a package.

write_image_node(ImgAttrs, Attrs, Stream, _Options) :-
    selectchk(src(Src), ImgAttrs, ImgAttrs1),
    (   Src = icons(_)
    ->  absolute_file_name(Src, AbsFile, [access(read)]),
        working_directory(CWD, CWD),
        relative_file_name(AbsFile, CWD, File)
    ;   url_cache(Src, File, _MimeType)
    ),
    filter_attributes(Attrs, td, TDAttrs, _Attrs1),
    html_current_option(dialect(Dialect)),
    html_set_options([dialect(xhtml)]),
    label_row(Attrs, Extra),
    option(border(Border), Attrs),
    phrase(html(table(border(Border),
                      [ tr(td(TDAttrs, img([src(File)|ImgAttrs1], [])))
                      | Extra
                      ])),
           Tokens),
    html_set_options([dialect(Dialect)]),
    with_output_to(string(HTML), print_html(Tokens)),
    write_attributes([html(HTML),shape(plaintext)], Stream).

label_row(Attrs, Extra) :-
    option(label(Label), Attrs),
    !,
    Extra = [tr(td([align(center)], Label))].
label_row(_, []).


%!  resource_label(+Resource, -Label:atom, +Options) is det.
%
%   Label is the textual label to show for Resource. Process the
%   options
%
%       * lang(+Lang)
%       * max_label_length(+Len)

resource_label(Resource, Label, Options) :-
    option(label_hook(Hook), Options),
    option(lang(Lang), Options, _),
    option(max_label_length(MaxLen), Options, 25),
    call(Hook, Resource, Lang, MaxLen, Label),
    !.
resource_label(Resource, Label, Options) :-
    option(lang(Lang), Options, _),
    rdf_display_label(Resource, Lang, Text),
    option(max_label_length(MaxLen), Options, 25),
    truncate_atom(Text, MaxLen, Label).



%!  write_attributes(+Attributes:list, +Out:stream) is det.
%
%   Write attribute values.  We define some special attributes:
%
%           * html(HTML)
%           Emit as label=<HTML>

write_attributes([], Out) :-
    !,
    format(Out, ' []').
write_attributes(List, Out) :-
    !,
    format(Out, ' [', []),
    write_attributes_2(List, Out),
    format(Out, ']', []).

write_attributes_2([], _).
write_attributes_2([H|T], Out) :-
    (   string_attribute(H)
    ->  H =.. [Att, Value],
        c_escape(Value, String),
        format(Out, ' ~w="~s"', [Att, String])
    ;   html_attribute(H, Att)
    ->  arg(1, H, Value),
        format(Out, ' ~w=<~s>', [Att, Value])
    ;   H =.. [Name, Value],
        format(Out, ' ~w=~w', [Name, Value])
    ),
    write_attributes_2(T, Out).


string_attribute(label(_)).
string_attribute(url(_)).
string_attribute(href(_)).
string_attribute(id(_)).
string_attribute('URL'(_)).
string_attribute(fillcolor(_)).
string_attribute(tooltip(_)).
string_attribute(style(_)).

html_attribute(html(_), label).


c_escape(Atom, String) :-
    atom_codes(Atom, Codes),
    phrase(cstring(Codes), String).

%!  filter_attributes(+AllAttrs, +Element,
%!                    -ForElement, -Rest) is det.

filter_attributes([], _, [], []).
filter_attributes([H|T], E, ForE, Rest) :-
    (   H =.. [Name,Value],
        gv_attr(Name, E, Type),
        is_of_type(Type, Value)
    ->  ForE = [H|R],
        filter_attributes(T, E, R, Rest)
    ;   Rest = [H|R],
        filter_attributes(T, E, ForE, R)
    ).

%!  gv_attr(?AttrName, ?Element, ?Type) is nondet.
%
%   Name and type-declarations for GraphViz   attributes.  Types are
%   defined my must_be/2.
%
%   @see http://www.graphviz.org/doc/info/shapes.html

gv_attr(align,        table, oneof([center,left,right])).
gv_attr(bgcolor,      table, atom).
gv_attr(border,       table, atom).
gv_attr(cellborder,   table, atom).
gv_attr(cellpadding,  table, atom).
gv_attr(cellspacing,  table, atom).
gv_attr(color,        table, atom).
gv_attr(fixedsize,    table, boolean).
gv_attr(height,       table, atom).
gv_attr(href,         table, atom).
gv_attr(port,         table, atom).
gv_attr(target,       table, atom).
gv_attr(title,        table, atom).
gv_attr(tooltip,      table, atom).
gv_attr(valign,       table, oneof([middle,bottom,top])).
gv_attr(width,        table, atom).

gv_attr(align,        td,    oneof([center,left,right,text])).
gv_attr(balign,       td,    oneof([center,left,right])).
gv_attr(bgcolor,      td,    atom).
gv_attr(border,       td,    atom).
gv_attr(cellpadding,  td,    atom).
gv_attr(cellspacing,  td,    atom).
gv_attr(color,        td,    atom).
gv_attr(colspan,      td,    integer).
gv_attr(fixedsize,    td,    boolean).
gv_attr(height,       td,    atom).
gv_attr(href,         td,    atom).
gv_attr(port,         td,    atom).
gv_attr(rowspan,      td,    integer).
gv_attr(target,       td,    atom).
gv_attr(title,        td,    atom).
gv_attr(tooltip,      td,    atom).
gv_attr(valign,       td,    oneof([middle,bottom,top])).
gv_attr(width,        td,    atom).

gv_attr(color,        font,  atom).
gv_attr(face,         font,  atom).
gv_attr('point-size', font,  integer).

gv_attr(align,        br,    oneof([center,left,right])).

gv_attr(scale,        img,   oneof([false,true,width,height,both])).
gv_attr(src,          img,   atom).


%!  cstring(+Codes)//
%
%   Create a C-string. Normally =dot=  appears   to  be  using UTF-8
%   encoding. Would there be a  safer   way  to  transport non-ascii
%   characters, such as \uXXXX?

cstring([]) -->
    [].
cstring([H|T]) -->
    (   cchar(H)
    ->  []
    ;   [H]
    ),
    cstring(T).

cchar(0'") --> "\\\"".
cchar(0'\n) --> "\\n".
cchar(0'\t) --> "\\t".
cchar(0'\b) --> "\\b".

wrap_url(URL0, URL, Options) :-
    option(wrap_url(Wrap), Options),
    call(Wrap, URL0, URL),
    !.
wrap_url(URL, URL, _).


%!  bag_shape(+Members, -BagShape, +Options) is det.
%
%   Compute parameters for a bag of resources.

bag_shape(Members, Shape, Options) :-
    option(bag_shape_hook(Hook), Options),
    call(Hook, Members, Shape),
    !.
bag_shape(_, [], _).

%!  shape(+Resource, -Attributes, +Options) is det.
%
%   Shape is the shape of the node to use for Resource.  Shapes
%   can be modified in two ways:
%
%       * through the option shape_hook(Closure), which must
%       return a valid Attributes list for GraphViz
%       * By addings sub-properties of graphviz:styleParameter
%       to the class of the resource.  The value of this property
%       defines the attribute value, while the label defines the
%       attribute-name.

shape(Resource, Shape, Options) :-
    option(shape_hook(Hook), Options),
    call(Hook, Resource, Shape, Options),
    !.
shape(Resource, Shape, _Options) :-
    findall(Style, gv_style(Resource, Style), Shape),
    debug(gv, '~p: shape = ~q', [Resource, Shape]).

gv_style(R, Style) :-
    rdfs_individual_of(R, Class),
    gv_class_style(Class, Style).

gv_class_style(Class, Style) :-
    rdf_has(Class, graphviz:styleParameter, literal(V), P),
    rdf_has(P, rdfs:label, literal(A)),
    Style =.. [A,V].


%!  edge_attributes(+Triple, -Attributes, +Options) is det.
%
%   @arg Triple is a term rdf(S,P,O).

edge_attributes(Predicate, Attributes, Options) :-
    option(edge_hook(Hook), Options),
    debug(edge(hook), 'Hook = ~p', [Hook]),
    call(Hook, Predicate, Attributes, Options),
    !.
edge_attributes(_, [], _).


                 /*******************************
                 *         IMAGE SERVER         *
                 *******************************/

% These handlers are relative to the handler of send_graph.  Possibly
% it would be better to merge that code.

:- http_handler(root('graphviz/cache/url/'), cached_image_in_svg, [prefix]).
:- http_handler(root('graphviz/'),           local_image_in_svg,  [prefix]).

%!  cached_image_in_svg(+Request)
%
%   HTTP handler to serve an image we have included in an SVG file.
%
%   @tbd    Should we restrict files served to files that are part of
%           recently served SVG files?

cached_image_in_svg(Request) :-
    memberchk(path_info(PathInfo), Request),
    atom_concat('cache/url/', PathInfo, File),
    url_cached(URL, file(File)),
    url_cached(URL, mime_type(MimeType)),
    http_reply_file(File,
                    [ mime_type(MimeType),
                      unsafe(true)
                    ],
                    Request).

local_image_in_svg(Request) :-
    memberchk(path_info(PathInfo), Request),
    file_base_name(PathInfo, ImageFile),
    http_reply_file(icons(ImageFile), [], Request).



                 /*******************************
                 *   RDF BASED CUSTOMIZATION    *
                 *******************************/

:- rdf_meta
    exclude_triple(r,r,o).

exclude_triple(rdf(S,P,O)) :-
    exclude_triple(S,P,O).

exclude_triple(_,rdf:type,C) :-
    rdf_has(C, graphviz:hideType, literal(type(xsd:boolean, true))).
