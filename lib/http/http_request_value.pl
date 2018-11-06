/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2018, VU University Amsterdam
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

:- module(http_request_value,
          [ http_parse_header_value/3   % +Header, +HeaderValue, -MediaTypes
          ]).
:- use_module(library(dcg/basics)).
:- use_module(library(pairs)).
:- use_module(library(lists)).

/** <module> Analyse values from HTTP request fields

The HTTP server parses some common  fields   in  the request header into
meaningful Prolog terms. Some fields however are not frequently used and
rather hard to parse. This library is intended  to grow in to a complete
library for processing header fields.

@tbd    Clients will frequently use same the value for many of these
        fields.  It is probably worthwhile to maintain a cache of
        translations.
*/

%!  http_parse_header_value(+Field, +Value, -Prolog) is semidet.
%
%   Translate Value in a meaningful Prolog   term. Field denotes the
%   HTTP request field for which we   do  the translation. Supported
%   fields are:
%
%     * accept
%
%   @error  domain_error(http_request_field, Field) if this
%           library is not prepared to handle this field (yet).

http_parse_header_value(accept, Value, Prolog) :-
    !,
    http_parse_accept_header(Value, Prolog).
http_parse_header_value(Field, _, _) :-
    domain_error(http_request_field, Field).

%!  http_parse_accept_header(+Header, -Fields) is semidet.
%
%   Parse an =|Accept:|=  header,  returning   a  list  of  accepted
%   media-type sorted by quality. If   multiple media-types have the
%   same quality, the most specific appears   first.  Each member of
%   the list is a term
%
%       media(Type/Subtype, ListOfParameters, Quality, AcceptExtension)
%
%   Type and/or Subtype is a variable if the specified value is =|*|=.
%
%   @see http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html

http_parse_accept_header(Header, Media) :-
    atom_codes(Header, Codes),
    phrase(accept(Media0), Codes),
    keysort(Media0, Media1),
    pairs_values(Media1, MediaR),
    reverse(MediaR, Media).

accept([H|T]) -->
    blanks,
    media_range(H),
    blanks,
    (   ","
    ->  accept(T)
    ;   {T=[]}
    ).

media_range(s(Quality,Spec)-media(Type, TypeParams, Quality, AcceptExts)) -->
    media_type(Type),
    blanks,
    (   ";"
    ->  blanks,
        parameters_and_quality(TypeParams, Quality, AcceptExts)
    ;   { TypeParams = [],
          Quality = 1.0,
          AcceptExts = []
        }
    ),
    { rank_specialised(Type, TypeParams, Spec) }.


%!  rank_specialised(+Type, +TypeParam, -Key) is det.
%
%   Although the specification linked  above   is  unclear, it seems
%   that  more  specialised  types  must   be  preferred  over  less
%   specialized ones.
%
%   @tbd    Is there an official specification of this?

rank_specialised(Type/SubType, TypeParams, v(VT, VS, VP)) :-
    var_or_given(Type, VT),
    var_or_given(SubType, VS),
    length(TypeParams, VP).

var_or_given(V, Val) :-
    (   var(V)
    ->  Val = 0
    ;   Val = 1
    ).

media_type(Type/SubType) -->
    type(Type), "/", type(SubType).

type(_) -->
    "*",
    !.
type(Type) -->
    token(Type).

parameters_and_quality(Params, Quality, AcceptExts) -->
    token(Name),
    blanks, "=", blanks,
    (   { Name == q }
    ->  float(Quality), blanks,
        accept_extensions(AcceptExts),
        { Params = [] }
    ;   { Params = [Name=Value|T] },
        parameter_value(Value),
        blanks,
        (   ";"
        ->  blanks,
            parameters_and_quality(T, Quality, AcceptExts)
        ;   { T = [],
              Quality = 1.0,
              AcceptExts = []
            }
        )
    ).

accept_extensions([H|T]) -->
    ";",
    !,
    blanks, token(Name), blanks,
    (   "="
    ->  blanks,
        (   token(Value)
        ->  []
        ;   quoted_string(Value)
        ),
        { H = (Name=Value) }
    ;   { H = Name }
    ),
    blanks,
    accept_extensions(T).
accept_extensions([]) -->
    [].

parameter_value(Value) -->
    (   token(Value)
    ->  []
    ;   quoted_string(Value)
    ).


%!  token(-Name)// is semidet.
%
%   Process an HTTP header token from the input.

token(Name) -->
    token_char(C1),
    token_chars(Cs),
    { atom_codes(Name, [C1|Cs]) }.

token_chars([H|T]) -->
    token_char(H),
    !,
    token_chars(T).
token_chars([]) --> [].

token_char(C) -->
    [C],
    {   \+ ctl(C),
        \+ separator_code(C)
    },
    !.

ctl(C) :- between(0,31,C), !.
ctl(127).

separator_code(0'().
separator_code(0')).
separator_code(0'<).
separator_code(0'>).
separator_code(0'@).
separator_code(0',).
separator_code(0';).
separator_code(0':).
separator_code(0'\\).
separator_code(0'").
separator_code(0'/).
separator_code(0'[).
separator_code(0']).
separator_code(0'?).
separator_code(0'=).
separator_code(0'{).
separator_code(0'}).
separator_code(32).
separator_code(0'\t).


%!  quoted_string(-Text)// is semidet.
%
%   True if input starts with a quoted string representing Text.

quoted_string(Text) -->
    "\"",
    quoted_text(Codes),
    { atom_codes(Text, Codes) }.

quoted_text([]) -->
    "\"",
    !.
quoted_text([H|T]) -->
    "\\", !, [H],
    quoted_text(T).
quoted_text([H|T]) -->
    [H],
    !,
    quoted_text(T).
