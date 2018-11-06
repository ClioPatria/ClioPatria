/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2018, University of Amsterdam,
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

:- module(rdf_file_type,
          [ rdf_guess_data_format/2,    % +Stream, ?Format
            rdf_guess_format_and_load/2 % +Stream, +Options
          ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(memfile)).
:- use_module(library(sgml)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- if(exists_source(library(archive))).
:- use_module(library(archive)).
:- endif.

/** <module> Load RDF data from unknown file-type

*/


%!  rdf_guess_format_and_load(+Stream, +Options) is det.
%
%   Guess the RDF format in Stream  and   load  it. Stream must be a
%   _repositional_ stream. Options are  passed   to  rdf_load/2.  In
%   addition, it processed the following options:
%
%     - filename(filename)
%     Name of the uploaded file.

rdf_guess_format_and_load(Stream, Options) :-
    option(format(_), Options),
    !,
    rdf_load(stream(Stream), Options).
:- if(current_predicate(archive_data_stream/3)).
rdf_guess_format_and_load(Stream, Options) :-
    setup_call_cleanup(
        archive_open(Stream, Archive, [format(all),format(raw)]),
        forall(archive_data_stream(Archive, DataStream, [meta_data(MetaData)]),
               call_cleanup(
                   ( member_base_uri(MetaData, Options, Options2),
                     option(base_uri(Base), Options2, 'http://example.org/'),
                     set_stream(DataStream, file_name(Base)),
                     (   file_base_name(Base, FileName),
                         non_rdf_file(FileName)
                     ->  true
                     ;   rdf_guess_data_format(DataStream, Format)
                     ->  rdf_load(stream(DataStream), [format(Format)|Options2])
                     ;   true
                     )
                   ),
                   close(DataStream))),
        archive_close(Archive)).

member_base_uri([_], Options, Options) :- !.
member_base_uri(MetaData, Options0, Options) :-
    append(MetaPath, [_], MetaData),
    maplist(get_dict(name), MetaPath, MetaSegments),
    select_option(base_uri(Base0), Options0, Options1, 'http://archive.org'),
    atomic_list_concat([Base0|MetaSegments], /, Base),
    Options = [base_uri(Base)|Options1].
:- else.
rdf_guess_format_and_load(Stream, Options) :-
    rdf_guess_data_format(Stream, Format),
    rdf_load(stream(Stream), [format(Format)|Options]).
:- endif.

non_rdf_file(File) :-
    file_name_extension(Base, Ext, File),
    (   non_rdf_ext(Ext)
    ->  true
    ;   downcase_atom(Base, Lower),
        non_rdf_base(Lower)
    ).

non_rdf_ext(pdf).
non_rdf_ext(txt).
non_rdf_ext(md).
non_rdf_ext(doc).

non_rdf_base(readme).
non_rdf_base(todo).

%!  rdf_guess_data_format(+Stream, ?Format)
%
%   Guess the format  of  an  RDF   file  from  the  actual content.
%   Currently, this seeks for a valid  XML document upto the rdf:RDF
%   element before concluding that the file is RDF/XML. Otherwise it
%   assumes that the document is Turtle.
%
%   @tbd    Recognise Turtle variations from content

rdf_guess_data_format(_, Format) :-
    nonvar(Format),
    !.
rdf_guess_data_format(Stream, xml) :-
    xml_doctype(Stream, _),
    !.
rdf_guess_data_format(Stream, Format) :-
    stream_property(Stream, file_name(File)),
    file_name_extension(_, Ext, File),
    rdf_db:rdf_file_type(Ext, Format),
    !.
rdf_guess_data_format(_, turtle).


%!  xml_doctype(+Stream, -DocType) is semidet.
%
%   Parse a stream and get the name   of the first XML element *and*
%   demand that this element defines XML   namespaces.  Fails if the
%   document is illegal XML before the first element.
%
%   Note that it is not  possible   to  define valid RDF/XML without
%   namespaces, while it is not possible  to define a valid absolute
%   Turtle URI (using <URI>) with a valid xmlns declaration.
%
%   @arg Stream denotes the input. If peek_string/3 is provided
%   (SWI-Prolog version 7), it is not necessary that the stream can
%   be repositioned. Older versions require a repositionable stream.

:- if(current_predicate(peek_string/3)).
xml_doctype(Stream, DocType) :-
    peek_string(Stream, 4096, Start),
    setup_call_cleanup(
        open_string_stream(Start, In),
        xml_doctype_2(In, DocType),
        close(In)).
:- else.
xml_doctype(Stream, DocType) :-
    xml_doctype_2(Stream, DocType).
:- endif.

xml_doctype_2(Stream, DocType) :-
    catch(setup_call_cleanup(make_parser(Stream, Parser, State),
                             sgml_parse(Parser,
                                        [ source(Stream),
                                          max_errors(1),
                                          syntax_errors(quiet),
                                          call(begin, on_begin),
                                          call(cdata, on_cdata)
                                        ]),
                             cleanup_parser(Stream, Parser, State)),
          E, true),
    nonvar(E),
    E = tag(DocType).

make_parser(Stream, Parser, state(Pos)) :-
    stream_property(Stream, position(Pos)),
    new_sgml_parser(Parser, []),
    set_sgml_parser(Parser, dialect(xmlns)).

cleanup_parser(Stream, Parser, state(Pos)) :-
    free_sgml_parser(Parser),
    set_stream_position(Stream, Pos).

on_begin(Tag, Attributes, _Parser) :-
    memberchk(xmlns:_=_, Attributes),
    throw(tag(Tag)).

on_cdata(_CDATA, _Parser) :-
    throw(error(cdata)).


open_string_stream(String, Stream) :-
    new_memory_file(MF),
    setup_call_cleanup(
        open_memory_file(MF, write, Out),
        format(Out, '~s', [String]),
        close(Out)),
    open_memory_file(MF, read, Stream,
                     [ free_on_close(true)
                     ]).
