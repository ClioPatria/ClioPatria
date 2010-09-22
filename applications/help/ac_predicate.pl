/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, VU University Amsterdam

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

:- module(autocomplete_predicates,
	  [
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/yui_resources)).
:- use_module(library(pldoc/doc_html)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(occurs)).

:- multifile
	prolog:doc_search_field//1.

:- http_handler(root(autocomplete/ac_predicate), ac_predicate, []).

max_results_displayed(100).

%	prolog:doc_search_field(+Options) is det.

prolog:doc_search_field(Options) -->
	{ select_option(size(W), Options, Options1),
	  atomic_concat(W, ex, Wem),
	  max_results_displayed(Max)
	},
	autocomplete(ac_predicate,
		     [ query_delay(0.3),
		       auto_highlight(false),
		       max_results_displayed(Max),
		       width(Wem)
		     | Options1
		     ]).

%%	autocomplete(+HandlerID, +Options)// is det.
%
%	Insert a YUI autocomplete widget that obtains its alternatives
%	from HandlerID.  The following Options are supported:
%
%	    * width(+Width)
%	    Specify the width of the box.  Width must satisfy the CSS
%	    length syntax.
%
%	    * query_delay(+Seconds)
%	    Wait until no more keys are typed for Seconds before sending
%	    the query to the server.

autocomplete(Handler, Options) -->
	{ http_location_by_id(Handler, Path),
	  atom_concat(Handler, '_complete', CompleteID),
	  atom_concat(Handler, '_input', InputID),
	  atom_concat(Handler, '_container', ContainerID),
	  select_option(width(Width), Options, Options1, '25em'),
	  select_option(name(Name), Options1, Options2, predicate),
	  select_option(value(Value), Options2, Options3, '')
	},
	html([ \html_requires(yui('autocomplete/autocomplete.js')),
	       \html_requires(yui('autocomplete/assets/skins/sam/autocomplete.css')),
	       div(id(CompleteID),
		   [ input([ id(InputID),
			     name(Name),
			     value(Value),
			     type(text)
			   ]),
		     div(id(ContainerID), [])
		   ]),
	       style(type('text/css'),
		     [ '#', CompleteID, '\n',
		       '{ width:~w; padding-bottom:0em; display:inline-block; vertical-align:top}'-[Width]
		     ]),
	       \autocomplete_script(Path, InputID, ContainerID, Options3)
	     ]).

autocomplete_script(HandlerID, Input, Container, Options) -->
	{ http_absolute_location(HandlerID, Path, [])
	},
	html(script(type('text/javascript'), \[
'{ \n',
'  var oDS = new YAHOO.util.XHRDataSource("~w");\n'-[Path],
'  oDS.responseType = YAHOO.util.XHRDataSource.TYPE_JSON;\n',
'  oDS.responseSchema = { resultsList:"results",
			  fields:["label","type","href"]
			};\n',
'  oDS.maxCacheEntries = 5;\n',
'  var oAC = new YAHOO.widget.AutoComplete("~w", "~w", oDS);\n'-[Input, Container],
'  oAC.resultTypeList = false;\n',
'  oAC.formatResult = function(oResultData, sQuery, sResultMatch) {
     var into = "<span class=\\"acmatch\\">"+sQuery+"</span>";
     var sLabel = oResultData.label.replace(sQuery, into);
     return "<span class=\\"" + oResultData.type + "\\">" + sLabel + "</span>";
   };\n',
'  oAC.itemSelectEvent.subscribe(function(sType, aArgs) {
     var oData = aArgs[2];
     window.location.href = oData.href;
   });\n',
\ac_options(Options),
'}\n'
					     ])).
ac_options([]) -->
	[].
ac_options([H|T]) -->
	ac_option(H),
	ac_options(T).

ac_option(query_delay(Time)) --> !,
	html([ '  oAC.queryDelay = ~w;\n'-[Time] ]).
ac_option(auto_highlight(Bool)) --> !,
	html([ '  oAC.autoHighlight = ~w;\n'-[Bool] ]).
ac_option(max_results_displayed(Max)) -->
	html([ '  oAC.maxResultsDisplayed = ~w;\n'-[Max] ]).
ac_option(O) -->
	{ domain_error(yui_autocomplete_option, O) }.

%%	ac_predicate(+Request)
%
%	HTTP handler for completing a predicate-name.   The  output is a
%	JSON object that describes possible completions.

ac_predicate(Request) :-
	max_results_displayed(DefMax),
	http_parameters(Request,
			[ query(Query, [ description('Typed string') ]),
			  maxResultsDisplayed(Max,
					      [ integer, default(DefMax),
						description('Max number of results to show')
					      ])
			]),
	autocompletions(Query, Max, Count, Completions),
	reply_json(json([ query = json([ count=Count
				       ]),
			  results = Completions
			])).

autocompletions(Query, Max, Count, Completions)  :-
	autocompletions(name, Query, Max, BNC, ByName),
	(   BNC > Max
	->  Completions = ByName,
	    Count = BNC
	;   TMax is Max-BNC,
	    autocompletions(token, Query, TMax, BTC, ByToken),
	    append(ByName, ByToken, Completions),
	    Count is BNC+BTC
	).

autocompletions(How, Query, Max, Count, Completions) :-
	findall(C, ac_object(How, Query, C), Completions0),
	sort(Completions0, Completions1),
	length(Completions1, Count),
	first_n(Max, Completions1, Completions2),
	maplist(obj_result, Completions2, Completions).

obj_result(_Name-Obj, json([ label=Label,
			     type=Type,
			     href=Href
			   ])) :-
	obj_name(Obj, Label, Type),
	object_href(Obj, Href).

obj_name(c(Function), Name, cfunc) :- !,
	atom_concat(Function, '()', Name).
obj_name(M:Term, Name, Class) :- !,
	predicate_class(M:Term, Class),
	format(atom(Name), '<span class="ac-module">~w</span>:~w', [M,Term]).
obj_name(Term, Name, 'ac-builtin') :-
	format(atom(Name), '~w', [Term]).

predicate_class(Head, built_in) :-
	predicate_property(Head, 'ac-builtin'), !.
predicate_class(Head, exported) :-
	predicate_property(Head, 'ac-exported'), !.
predicate_class(Head, hook) :-
	predicate_property(Head, 'ac-multifile'), !.
predicate_class(_, 'ac-private').


first_n(0, _, []) :- !.
first_n(_, [], []) :- !.
first_n(N, [H|T0], [H|T]) :-
	N2 is N - 1,
	first_n(N2, T0, T).


		 /*******************************
		 *	  PREFIX DATABASE	*
		 *******************************/

ac_object(name, Prefix, Name-Obj) :-
	prefix_index(ByName, _ByToken),
	rdf_keys_in_literal_map(ByName, prefix(Prefix), Keys),
	member(Name, Keys),
	name_object(Name, Obj, _Category).
ac_object(token, Prefix, Name-Obj) :-
	prefix_index(_ByName, ByToken),
	rdf_keys_in_literal_map(ByToken, prefix(Prefix), Keys),
	member(Token, Keys),
	rdf_find_literal_map(ByToken, [Token], Names),
	member(Name, Names),
	name_object(Name, Obj, _Category).


:- dynamic
	prefix_map/2,			% name-map, token-map
	name_object/3.

prefix_index(ByName, ByToken) :-
	prefix_map(ByName, ByToken), !.
prefix_index(ByName, ByToken) :-
	rdf_new_literal_map(ByName),
	rdf_new_literal_map(ByToken),
	assertz(prefix_map(ByName, ByToken)),
	fill_token_map.

fill_token_map :-
	prefix_map(ByName, ByToken),
	rdf_reset_literal_map(ByName),
	rdf_reset_literal_map(ByToken),
	retractall(name_object(_,_,_)),
	(   documented(Obj, Category),
	    completion_target(Obj, Name),
	    assertz(name_object(Name, Obj, Category)),
	    rdf_insert_literal_map(ByName, Name, Name),
	    forall(start_inside_token(Name, Token),
		   rdf_insert_literal_map(ByToken, Token, Name)),
	    fail
	;   true
	),
	keep_best_doc.

documented(Obj, Category) :-
	prolog:doc_object_summary(Obj, Category, _Section, _Summary).

keep_best_doc :-
	(   name_object(Name, Obj, Category),
	    name_object(Name, Obj2, Category2),
	    same_object(Obj, Obj2),
	    better_category(Category2, Category),
	    retract(name_object(Name, Obj, Category)),
	    fail
	;   true
	).

same_object(_:Name/Arity, Name/Arity).
same_object(Name/Arity, _:Name/Arity).

better_category(manual, _) :- !.
better_category(packages, _) :- !.


completion_target(Name/_,    Name).
completion_target(Name//_,   Name).
completion_target(_:Name/_,  Name).
completion_target(_:Name//_, Name).
%completion_target(c(Name),  Name).

start_inside_token(Token, Inside) :-
	sub_atom(Token, _, _, L, '_'),
	sub_atom(Token, _, L, 0, Inside).
