/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam,
		   VU University Amsterdam

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

:- module(cp_simple_search,
	  [ simple_search_form//0
	  ]).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

:- use_module(library(semweb/rdfs)).

:- use_module(library(option)).

:- use_module(label).

:- http_handler(api(ac_find_literal), ac_find_literal, []).

/** <module> Simple literal search
*/

%%	simple_search_form//
%
%	Provide a search form to find literals in the database.

simple_search_form -->
	html_requires(css('rdf_browse.css')),
	html(form([ id(search_form),
		    action(location_by_id(search))
		  ],
		  [ div([ \search_box([ name(q) ]),
			  input([ type(submit),
				  value('Search')
				])
			])
		  ])).

max_results_displayed(100).

search_box(Options) -->
	{ max_results_displayed(Max)
	},
	autocomplete(ac_find_literal,
		     [ query_delay(0.2),
		       auto_highlight(false),
		       max_results_displayed(Max),
		       width('30ex')
		     | Options
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

highlight -->
	html(script(type('text/javascript'),
\[
  'function highlighMatches(str, query, cls)\n',
  '{ var pat = new RegExp(query, "gi");
     var sa = str.split(pat);
     var ma = str.match(pat);
     var i;
     var out = sa[0];\n',

  '  if ( !ma )
     { return str;
     }\n',

  '  for(i=0; i<ma.length; )
     { out += "<span class=\'"+cls+"\'>"+ma[i++]+"</span>";
       out += sa[i];
     }\n',

  'return out;
   }\n'
 ])).

autocomplete_script(HandlerID, Input, Container, Options) -->
	{ http_absolute_location(HandlerID, Path, [])
	},
	highlight,
	html(script(type('text/javascript'), \[
'{ \n',
'  var oDS = new YAHOO.util.XHRDataSource("~w");\n'-[Path],
'  oDS.responseType = YAHOO.util.XHRDataSource.TYPE_JSON;\n',
'  oDS.responseSchema = { resultsList:"results",
			  fields:["label","count","href"]
			};\n',
'  oDS.maxCacheEntries = 5;\n',
'  var oAC = new YAHOO.widget.AutoComplete("~w", "~w", oDS);\n'-[Input, Container],
'  oAC.resultTypeList = false;\n',
'  oAC.formatResult = function(oResultData, sQuery, sResultMatch) {
     var sLabel = highlighMatches(oResultData.label, sQuery, "acmatch");
     if ( oResultData.count > 1 ) {
       sLabel += " <span class=\\"account\\">("+oResultData.count+")</span>";
     }
     return sLabel;
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

%%	ac_find_literal(+Request)
%
%	Perform autocompletion for literals and  resources. The reply is
%	a JSON object that  is  normally   used  in  a  YUI autocomplete
%	widget.

ac_find_literal(Request) :-
	max_results_displayed(DefMax),
	http_parameters(Request,
			[ query(Query,
				[ description('Prefix for literals to find')
				]),
			  maxResultsDisplayed(Max,
					      [ integer, default(DefMax),
						description('Maximum number of results displayed')
					      ])
			]),
	autocompletions(Query, Max, Count, Completions),
	reply_json(json([ query = json([ count=Count
				       ]),
			  results = Completions
			])).

autocompletions(Query, Max, Count, Completions)  :-
	autocompletions(prefix(label), Query, Max, BNC, ByName),
	(   BNC > Max
	->  Completions = ByName,
	    Count = BNC
	;   TMax is Max-BNC,
	    autocompletions(prefix(other), Query, TMax, BTC, ByToken),
	    append(ByName, ByToken, Completions),
	    Count is BNC+BTC
	).

autocompletions(How, Query, Max, Count, Completions) :-
	ac_objects(How, Query, Completions0),
	length(Completions0, Count),
	first_n(Max, Completions0, Completions1),
	maplist(obj_result, Completions1, Completions).

obj_result(Text-Count,
	   json([ label=Text,
		  count=Count,
		  href=Href
		])) :-
	object_href(Text, Href).

object_href(Text, Link) :- !,
	term_to_atom(literal(Text), Atom),
	http_link_to_id(list_triples_with_object, [ l=Atom ], Link).

first_n(0, _, []) :- !.
first_n(_, [], []) :- !.
first_n(N, [H|T0], [H|T]) :-
	N2 is N - 1,
	first_n(N2, T0, T).


%%	ac_objects(+How, +Query, -Objects)
%
%	@param Objects is a list of Text-Count pairs

ac_objects(How, Query, Objects) :-
	findall(Pair, ac_object(How, Query, Pair), Pairs),
	keysort(Pairs, KSorted),
	group_pairs_by_key(KSorted, Grouped),
	maplist(hit_count, Grouped, Objects).

hit_count(Text-Resources, Text-Count) :-
	length(Resources, Count).	% duplicates?


%%	ac_object(+How, +Query, -Object)

ac_object(prefix(label), Query, Text-Resource) :-
	rdf(Resource, P, literal(prefix(Query), Literal)),
	(   label_property(LP),
	    rdfs_subproperty_of(P, LP)
	->  text_of_literal(Literal, Text)
	).
ac_object(prefix(other), Query, Text-Resource) :-
	rdf(Resource, P, literal(prefix(Query), Literal)),
	(   label_property(LP),
	    rdfs_subproperty_of(P, LP)
	->  fail
	;   text_of_literal(Literal, Text)
	).


