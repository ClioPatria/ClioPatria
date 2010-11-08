/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010 VU University Amsterdam

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

:- module(html_basics,
	  [ hidden//2,			% +Name, +Value
	    form_input//2,		% +Label, +Input
	    form_submit//1,		% +Label
	    n//2,			% +Format, +Value
	    nc//2,			% +Format, +Value
	    nc//3,			% +Format, +Value, +Options
	    odd_even_row//3,		% +Row, -Next, :Content
	    insert_html_file//1		% +FileSpec
	  ]).
:- use_module(library(http/html_write)).
:- use_module(library(sgml)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(occurs)).

:- html_meta
	form_input(html, html, ?, ?).

/** <module> Simple Small HTML components
*/

		 /*******************************
		 *	       FORMS		*
		 *******************************/

%%	hidden(+Name, +Value)// is det.
%
%	Create a hidden input field with given name and value

hidden(Name, Value) -->
	html(input([ type(hidden),
		     name(Name),
		     value(Value)
		   ])).


%%	form_input(+Label, +Input)// is det.
%%	form_submit(+Label)// is det.
%
%	Building blocks for HTML forms. The  form itself is a two-column
%	table of class =form= with labels at  the left and inputs at the
%	right. These rules create rows for input and submit.

form_input(Label, Input) -->
	html(tr([ th(class(label), Label),
		  td(Input)
		])).


form_submit(Label) -->
	html(tr(class(buttons),
		[ th([align(right), colspan(2)],
		     input([ type(submit),
			     value(Label)
			   ]))
		])).


		 /*******************************
		 *	       TABLES		*
		 *******************************/

%%	nc(+Format, +Value)// is det.
%%	nc(+Format, +Value, +Options)// is det.
%
%	Numeric  cell.  The  value  is    formatted   using  Format  and
%	right-aligned in a table cell (td).
%
%	@param	Format is a (numeric) format as described by format/2 or
%		the constant =human=.  _Human_ formatting applies to
%		integers and prints then in abreviated (K,M,T) form,
%		e.g., 4.5M for 4.5 million.
%	@param	Options is passed as attributed to the =td= element.
%		Default alignment is =right=.

nc(Fmt, Value) -->
	nc(Fmt, Value, []).

nc(Fmt, Value, Options) -->
	{ class(Value, Class),
	  merge_options(Options,
			[ align(right),
			  class(Class)
			], Opts),
	  number_html(Fmt, Value, HTML)
	},
	html(td(Opts, HTML)).

class(Value, Class) :-
	(   integer(Value)
	->  Class = int
	;   float(Value)
	->  Class = float
	;   Class = value
	).


%%	odd_even_row(+Row, -Next, :Content)//
%
%	Create odd/even alternating table rows from a DCG.

:- if(current_predicate((html_meta)/1)).
:- html_meta(odd_even_row(+,-,html,?,?)).
:- else.
:- meta_predicate odd_even_row(+,-,:,?,?).
:- endif.

odd_even_row(Row, Next, Content) -->
	{ (   Row mod 2 =:= 0
	  ->  Class = even
	  ;   Class = odd
	  ),
	  Next is Row+1
	},
	html(tr(class(Class), Content)).


		 /*******************************
		 *	       NUMBERS		*
		 *******************************/

%%	n(+Format, +Value)//
%
%	HTML component to emit a number.
%
%	@see nc//2 for details.

n(Fmt, Value) -->
	{ number_html(Fmt, Value, HTML) },
	html(HTML).

number_html(human, Value, HTML) :-
	integer(Value), !,
	human_count(Value, HTML).
number_html(Fmt, Value, HTML) :-
	number(Value), !,
	HTML = Fmt-[Value].
number_html(_, Value, '~p'-[Value]).


human_count(Number, HTML) :-
	Number < 1024, !,
	HTML = '~d'-[Number].
human_count(Number, HTML) :-
	Number < 1024*1024, !,
	KB is Number/1024,
	digits(KB, N),
	HTML = '~*fK'-[N, KB].
human_count(Number, HTML) :-
	Number < 1024*1024*1024, !,
	MB is Number/(1024*1024),
	digits(MB, N),
	HTML = '~*fM'-[N, MB].
human_count(Number, HTML) :-
	TB is Number/(1024*1024*1024),
	digits(TB, N),
	HTML = '~*fT'-[N, TB].

digits(Count, N) :-
	(   Count < 100
	->  N = 1
	;   N = 0
	).


		 /*******************************
		 *	   INCLUDE FILES	*
		 *******************************/

%%	insert_html_file(+Specification)
%
%	Insert the content of an HTML   file  into the current document.
%	Only the content of the =body= element is included.

insert_html_file(Alias) -->
	{ absolute_file_name(Alias, Page, [access(read)]),
	  load_html_file(Page, DOM),
	  contains_term(element(body, _, Body), DOM),
	  Style = element(style, _, _),
	  findall(Style, sub_term(Style, DOM), Styles),
	  append(Styles, Body, Content)
	},
	html(Content).
