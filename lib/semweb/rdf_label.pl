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

:- module(rdf_label,
          [ rdf_label/2,                % +Resource, -Literal
            rdf_display_label/2,        % +Resource, -Text
            rdf_display_label/3,        % +Resource, +Lang, -Text
            literal_text/2,             % +Literal, -Text
            truncate_atom/3,            % +Atom, -MaxLen -Text
            label_property/1            % ?Property
          ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf11), [rdf_lexical_form/2]).
:- use_module(user(preferences)).


/** <module> Generate labels for RDF objects

This library deals with  a  common   problem  in  RDF applications: show
labels for resources and display literals.  There is no clear-cut answer
to this problem because there  are  too   many  options.  Think of e.g.,
language  preferences,  producing   summaries,    desired   rdfs/owl/...
reasoning. Therefore, this library provides the  required APIs a default
implementation and hooks that allow for dealing with the above mentioned
issues.
*/

:- multifile
    label_property/1,               % ?Resource
    label_hook/2,                   % +Resource, -Literal
    display_label_hook/3.           % +Resource, ?Lang, -Label

:- rdf_meta
    rdf_label(r,-),
    rdf_display_label(r,-),
    rdf_display_label(r,?,-),
    label_property(r).

                                        % this dependency is not ideal ...
:- rdf_register_ns(foaf, 'http://xmlns.com/foaf/0.1/').

%!  label_property(?Property) is nondet.
%
%   True if Property is  used  to   represent  labels.  The  default
%   definition defines SKOS (prefLabel,  altLabel,   DC  (title) and
%   rdfs:label. This predicate is defined as =multifile=.

label_property(skos:prefLabel).
label_property(foaf:name).
label_property(dc:title).
label_property(rdfs:label).
label_property(skos:altLabel).


%!  rdf_label(+R, -Label:literal) is nondet.
%
%   Label is a label for R.  This   predicate  first  calls the hook
%   label_hook/2. If this hook fails it produces all property-values
%   for the properties  defined  by   label_property/1  that  have a
%   literal value.

rdf_label(R, Label) :-
    (   label_hook(R, Label)
    *-> true
    ;   label_property(P),
        rdf_has(R, P, Label),
        rdf_is_literal(Label)
    ).



%!  rdf_display_label(+R, -Text:text) is det.
%
%   Provide a label for R in the   user's  default language. This is
%   the same as rdf_display_label(R, _, Label).
%
%   @see user_preference/2

rdf_display_label(R, Label) :-
    rdf_display_label(R, _, Label).

%!  rdf_display_label(+R, ?Lang, -Text:text) is det.
%
%   Label is the preferred label to display   the  resource R in the
%   language Lang. As a last resort, this predicates creates a label
%   from the URI R.  In that case, Lang is unified with =url=.

rdf_display_label(R, Lang, Label) :-
    rdf_real_label(R, Lang, Label),
    !.
rdf_display_label(Resource, url, String) :-
    (   after_char(Resource, '#', Local), Local \= ''
    ->  Label = Local
    ;   after_char(Resource, '/', Local), Local \= ''
    ->  Label = Local
    ;   Label = Resource
    ),
    atom_string(Label, String).


rdf_real_label(R, Lang, Label) :-
    % first compute label based on user-defined hook
    display_label_hook(R, Lang, Label),
    !.
rdf_real_label(R, Lang, Label) :-
    % compute label in given language Lang
    nonvar(Lang),
    rdf_is_resource(R),
    (   rdf_label(R, literal(lang(Lang, Label))) % Try fast option first
    ->  Literal = literal(lang(Lang, Label))
    ;   rdf_label(R, Literal),    % warning: BT over next call is expensive when R has labels in many languages:
        Literal = literal(lang(Lang0, Label)),
        lang_matches(Lang0, Lang)
    ),
    !,
    literal_text(Literal, Label).

rdf_real_label(R, Lang, Label) :-
    % compute label in user prefered language if Lang not given
    var(Lang),
    rdf_is_resource(R),
    user_preference(user:lang, literal(Lang)),
    rdf_real_label(R, Lang, Label).


rdf_real_label(R, Lang, Label) :-
    % compute label in any language, unify this language with Lang
    var(Lang),
    rdf_is_resource(R),
    rdf_label(R, Literal),
    literal_lang(Literal, Lang),
    literal_text(Literal, Label).

rdf_real_label(BNode, Lang, Label) :-
    rdf_has(BNode, rdf:value, Value),
    rdf_real_label(Value, Lang, Label0),
    !,
    format(atom(Label), '[~a..]', Label0).
rdf_real_label(Literal, Lang, Label) :-
    rdf_is_literal(Literal),
    !,
    literal_lang(Literal, Lang),
    literal_text(Literal, Label).

after_char(Atom, Char, Rest) :-
    State = last(-),
    (   sub_atom(Atom, _, _, L, Char),
        nb_setarg(1, State, L),
        fail
    ;   arg(1, State, L),
        L \== (-)
    ),
    sub_atom(Atom, _, L, 0, Rest).

literal_lang(literal(Lang0, _), Lang) :-
    !,
    Lang = Lang0.

literal_lang(literal(lang(Lang0, _)), Lang) :-
    !,
    Lang = Lang0.

literal_lang(_, _).

%!  literal_text(++Object, -Text:text) is semidet.
%
%   Text is the textual content of Object. Fails if Object is not an
%   RDF literal (term literal(Value)). If   Object is an XMLLiteral,
%   Text is unified with the XML-text.
%
%   @error instantiation_error if Object is not ground

literal_text(Literal, Text) :-
    ground(Literal),
    (   atom(Literal)
    ->  Text = Literal
    ;   string(Literal)
    ->  Text = Literal
    ;   rdf_lexical_form(Literal, Lexical),
        (   Lexical = @(Text, _Lang)
        ->  true
        ;   Lexical = ^^(Text, _Type)
        )
    ).

%!  truncate_atom(+Atom, +MaxLen, -Truncated) is det.
%
%   If Atom is longer than MaxLen, truncate  it. If MaxLen is =inf=,
%   Truncated is unified with Atom.

truncate_atom(Atom, inf, All) :-
    !,
    All = Atom.
truncate_atom(Atom, MaxLen, Truncated) :-
    atom_length(Atom, Len),
    (   Len =< MaxLen
    ->  Truncated = Atom
    ;   TLen is max(3, MaxLen-4),
        sub_atom(Atom, 0, TLen, _, S0),
        atom_concat(S0, ' ...', Truncated)
    ).

                 /*******************************
                 *            SANDBOX           *
                 *******************************/

:- multifile
    sandbox:safe_primitive/1.

sandbox:safe_primitive(rdf_label:literal_text(_,_)).
