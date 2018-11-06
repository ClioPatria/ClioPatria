/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2018, University of Amsterdam,
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

:- module(http_settings,
          [ http_show_settings/3,       % +Options
            http_apply_settings/4       % +Request, +Options
          ]).
:- use_module(library('http/html_write')).
:- use_module(library('http/html_head')).
:- use_module(library('http/http_parameters')).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(debug)).
:- use_module(library(settings)).

%!  http_show_settings(+Options)// is det
%
%   Emit an HTML representation of the current settings.  Options:
%
%       * edit(+Boolean)
%
%       * hide_module(+Boolean)
%       If true, hide module headers from the user.
%
%       * module(+ModuleList)
%       If present, only show settings from modules in ModuleList.

http_show_settings(Options) -->
    { findall(M-N, current_setting(M:N), List),
      keysort(List, Sorted),
      group_pairs_by_key(Sorted, ByModule)
    },
    (   { option(edit(true), Options, false),
          option(action(Action), Options, '/http/settings')
        }
    ->  html(form([ action(Action),
                    method('GET')
                  ],
                  table(class(block),
                        \settings_table(ByModule, Options))))
    ;   html([ table(class(block),
                     \settings_table(ByModule, Options))
             ])
    ).

settings_table(ByModule, Options) -->
    (   {ByModule = [M-List],
        \+ option(hide_module(_), Options)
        }
    ->  show_module(M, List, [hide_module(true)|Options])
    ;   show_modules(ByModule, Options)
    ),
    (   { option(edit(true), Options, true) }
    ->  html(tr(class(buttons),
                td([ colspan(2),
                     align(right)
                   ],
                   [ input([ type(reset) ]),
                     input([ type(submit), value('Apply') ])
                   ])))
    ;   []
    ).


show_modules([], _) -->
    [].
show_modules([M-List|T], Options) -->
    show_module(M, List, Options),
    show_modules(T, Options).

show_module(Module, _Settings, Options) -->
    { option(modules(ListOfModules), Options),
      \+ memberchk(Module,ListOfModules)
    },
    !.
show_module(Module, Settings, Options) -->
    show_module_header(Module, Options),
    show_settings(Settings, Module, odd, Options).

show_module_header(_Module, Options) -->
    { option(hide_module(true), Options, false)},
    !.
show_module_header(Module, _Options) -->
    html(tr(th([colspan(2), class(group)], Module))).

show_settings([], _, _, _) -->
    [].
show_settings([H|T], Module, EO, Options) -->
    show_setting(H, Module, EO, Options),
    { negate_odd_even(EO, EO2) },
    show_settings(T, Module, EO2, Options).

show_setting(H, Module, EO, Options) -->
    { setting_property(Module:H, comment(Comment)),
      setting_property(Module:H, type(Type)),
      setting_title(Module:H, Title),
      setting(Module:H, Value),
      debug(settings, '~w: type=~w', [H, Type])
    },
    html(tr(class(EO),
            [ td([class(comment), title(Title)], Comment),
              td(class(value),
                 \show_value(Type, Value, Module:H, Options))
            ])).

setting_title(Setting, Title) :-
    setting_property(Setting, File:Line),
    integer(Line),
    !,
    file_base_name(File, Base),
    format(atom(Title), '~q from ~w:~d', [Setting, Base, Line]).
setting_title(Setting, Title) :-
    format(atom(Title), '~q', [Setting]).


show_value(Type, Value, Id, Options) -->
    { option(edit(true), Options, true) },
    !,
    input_value(Type, Value, Id).
show_value(Type, Value, _, _Options) -->
    show_value(Type, Value).

%!  show_value(+Type, +Value)// is det.
%
%   Emit a Value in non-editable representation.

show_value(list(Type), Values) -->
    !,
    html(div(class(list), \show_list(Values, Type, odd))).
show_value(_, Value) -->
    html('~w'-[Value]).

show_list([], _, _) -->
    [].
show_list([H|T], Type, Class) -->
    html(div(class(elem_+Class), \show_value(Type, H))),
    { negate_odd_even(Class, NextClass) },
    show_list(T, Type, NextClass).


%!  input_value(+Type, +Value, +Id)// is det.
%
%   Emit an form-field for Value.

:- multifile
    input_item/5.                   % input_item(+Type, +Value, +Id)//

input_value(Type, Value, Id) -->
    { html_name(Id, Name) },
    (   input_item(Type, Value, Name)
    ->  []
    ;   builtin_input_item(Type, Value, Name)
    ).

builtin_input_item(boolean, Value, Name) -->
    !,
    builtin_input_item(oneof([true,false]), Value, Name).
builtin_input_item(between(L,U), Value, Name) -->
    !,
    html(input([ type(range),
                 name(Name),
                 min(L), max(U), value(Value)
               ])).
builtin_input_item(oneof(List), Value, Name) -->
    !,
    html(select([name(Name)], \oneof(List, Value))).
builtin_input_item(atom, Value, Name) -->
    !,
    html(input([name(Name), size(40), value(Value)])).
builtin_input_item(_, Value, Name) -->
    { format(string(S), '~q', [Value])
    },
    html(input([name(Name), size(40), value(S)])).

oneof([], _) -->
    [].
oneof([H|T], Value) -->
    (   {H == Value}
    ->  html([ option([selected(selected),value(H)], H) ])
    ;   html([ option([                   value(H)], H) ])
    ),
    oneof(T, Value).


                 /*******************************
                 *         APPLY SETTINGS       *
                 *******************************/

%!  http_apply_settings(+Request, +Options)// is det
%
%   Process  form  data  created   by  http_show_settings//1,  apply
%   changes to the settings and create   a  feedback page indicating
%   which settings have changed.  Options:
%
%           * save(Boolean)
%           If =true= and some settings have changed, call
%           save_settings/0.
%
%           * save_as(File)
%           If some settings have changed, call save_settings(File).
%           The option =save_as= overrules =save=.

http_apply_settings(Request, Options) -->
    { http_parameters(Request, [],
                      [ form_data(Data)
                      ]),
      debug(settings, 'Form data: ~p', [Data]),
      phrase(process_settings_form(Data), Changes)
    },
    report_changed(Changes, Options).


report_changed([], _) -->
    html(div(class(msg_informational), 'No changes')).
report_changed(L, _) -->
    { memberchk(error(_), L) },
    report_errors(L).
report_changed(L, Options) -->
    { length(L, N),
      forall(member(change(Id, _, Value), L),
             set_setting(Id, Value)),
      (   option(save_as(File), Options)
      ->  save_settings(File)
      ;   option(save(true), Options, true)
      ->  save_settings
      ;   true
      )
    },
    html(div(class(msg_informational), ['Changed ', N, ' settings'])).

report_errors([]) -->
    [].
report_errors([error(Error)|T]) -->
    report_error(Error),
    report_errors(T).
report_errors([_|T]) -->
    report_errors(T).

report_error(no_setting(Id)) -->
    { format(string(Name), '~w', [Id]) },
    html(div(class(msg_error),
             ['Setting ', Name, ' does not exist.'])).
report_error(bad_value(Id, RawValue)) -->
    { format(string(Name), '~w', [Id]) },
    html(div(class(msg_error),
             ['Wrong value for ', Name, ': ', RawValue])).


%!  process_settings_form(+FormData)//
%
%   Process the raw form data, producing a list holding terms of the
%   form:
%
%           * error(no_setting(Setting))
%           * error(bad_value(Setting, Value))
%           * change(Setting, Old, New)

process_settings_form([]) -->
    [].
process_settings_form([Name = Value|T]) -->
    (   { html_name(Setting, Name) }
    ->  process_form_field(Setting, Value)
    ;   [ error(no_setting(Name)) ]
    ),
    process_settings_form(T).

process_form_field(Id, RawValue) -->
    (   { setting_property(Id, type(Type)) }
    ->  (   { catch(convert_setting_text(Type, RawValue, Value), _, fail) }
        ->  { setting(Id, OldValue) },
            (   { Value == OldValue }
            ->  []
            ;   [change(Id, OldValue, Value)]
            )
        ;   [ error(bad_value(Id, RawValue))]
        )
    ;   [ error(no_setting(Id)) ]
    ).



                 /*******************************
                 *             UTIL             *
                 *******************************/

%!  html_name(+Settings, -Name) is det.
%!  html_name(-Settings, +Name) is det.
%
%   Convert between Module:Setting and Name for use in form-fields.

html_name(Module:Setting, Name) :-
    atomic_list_concat([Module, Setting], ':', Name).


%!  negate_odd_even(+OddEven, -EventOdd)

negate_odd_even(odd, even).
negate_odd_even(even, odd).
