/*  This file is part of ClioPatria.

    Author:	Jan Wielemaker <J.Wielemaker@uva.nl>
    HTTP:	http://e-culture.multimedian.nl/software/ClioPatria.shtml
    GITWEB:	http://eculture.cs.vu.nl/git/ClioPatria.git
    GIT:	git://eculture.cs.vu.nl/home/git/eculture/ClioPatria.git
    GIT:	http://eculture.cs.vu.nl/home/git/eculture/ClioPatria.git
    Copyright:  2005-2009, E-Culture/MultimediaN

    ClioPatria is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    ClioPatria is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ClioPatria.  If not, see <http://www.gnu.org/licenses/>.
*/

:- module(http_help,
	  [ page_documentation_link//1	% +Request
	  ]).
:- use_module(http_tree).
:- use_module(doc_components,
	      [ api_tester//2,
		init_api_tester//0
	      ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_json)).
:- use_module(library(http/js_write)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_parameters)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(apply)).
					% PlDoc interface
:- use_module(library(pldoc/doc_html)).
:- use_module(library(pldoc/doc_process)).

/** <module> Explore the running HTTP server

This module is part of the  SWI-Prolog web-developent infrastructure. It
documents the HTTP server using the reflexive capabilities of Prolog and
the server infrastructure. Self-documentation is enabled by loading this
module. The entry-point of this module is   located at the HTTP location
root(help/http), using the handler-identifier =http_help=.

In    addition,    this     module      provides     the     _component_
page_documentation_link//1, which shows a small   book  linking from the
displayed page to its documentation.
*/

:- http_handler(root(help/http),	     http_help,	      []).
:- http_handler(root(help/http_handler),     help_on_handler, []).
:- http_handler(root(help/http_ac_location), ac_location,     []).

%%	page_documentation_link(+Request)// is det.
%
%	Show a link to the documentation of the current page.

page_documentation_link(Request) -->
	{ memberchk(path(Path), Request),
	  http_link_to_id(http_help, [location=Path], HREF),
	  http_absolute_location(icons('doc.png'), IMG, [])
	},
	html(a([id('dev-help'), href(HREF)],
	       img([ alt('Developer help'),
		     title('Page documentation'),
		     src(IMG)
		   ]))).

%%	http_help(Request)
%
%	HTTP handler to explore the Prolog HTTP server

http_help(Request) :-
	http_parameters(Request,
			[ location(Start,
				   [ optional(true),
				     description('Display help on location')
				   ])
			]),
	http_current_host(Request, Host, Port, [global(true)]),
	(   Port == 80
	->  Authority = Host
	;   format(atom(Authority), '~w:~w', [Host, Port])
	),
	(   var(Start)
	->  Options = []
	;   Options = [ location(Start) ]
	),
	reply_html_page(cliopatria(http_help),
			title('Server help'),
			[ body(class('yui-skin-sam'),
			       [ h1(class(title), 'Server at ~w'-[Authority]),
				 \help_page(Options)
			       ])
			]).

%%	help_page(Options)//
%
%	Emit the tree and #http-help  for   holding  the description. We
%	need to include the requirements for   PlDoc here as the scripts
%	are not loaded through the innerHTML method.
%
%	Options:
%
%	    * location(Location)
%	    Initially open Location.

help_page(Options) -->
	{ tree_view_options(TreeOptions) },
	html([ \html_requires(css('httpdoc.css')),
	       \html_requires(pldoc),
	       \html_requires(js('api_test.js')),
	       div(id('http-tree'), \http_tree_view(TreeOptions)),
	       div(id('http-find'), \quick_find_div_content),
	       div(id('http-help'), \usage),
	       \script(Options),
	       \init_api_tester
	     ]).

tree_view_options(
[ labelClick('function(node) { helpNode(node) }')
]).

usage -->
	html([ h4('Usage'),
	       p([ 'This page finds HTTP paths (locations) served by this ',
		   'server.  You can find locations by browsing the hierarchy ',
		   'at the left or by entering a few characters from the ',
		   'path in the search box above.  Autocompletion will show ',
		   'paths that contain the typed string.'
		 ])
	     ]).


%%	script(+Options)// is det.
%
%	Emit JavScript code that gets  the   help  for the HTTP location
%	associated  with  _node_  and  displays    this  information  in
%	#http-help.

script(Options) -->
	{ http_link_to_id(help_on_handler, [], Handler)
	},
	html([ script(type('text/javascript'),
		      \[
'function helpNode(node)\n',
'{',
'  helpHTTP(node.data.path);\n',
'}\n\n',
'function helpHTTP(path)\n',
'{',
'  var callback =\n',
'  { success: function(o)\n',
'             {\n',
'		var content = document.getElementById("http-help");\n',
'		content.innerHTML = o.responseText;\n',
'             }\n',
'  }\n',
'  var sUrl = "~w?location=" + encodeURIComponent(path);\n'-[Handler],
'  var transaction = YAHOO.util.Connect.asyncRequest("GET", sUrl, callback, null);\n',
'}\n',
	       \start(Options)
		       ])
	     ]).

start(Options) -->
	{ option(location(Start), Options)
	}, !,
	js_call(helpHTTP(Start)).
start(_) --> [].


%%	help_on_handler(+Request)
%
%	Describe the HTTP handler for the given location.
%
%	@tbd	Include the output format by scanning for one of the
%		defined output handlers.

help_on_handler(Request) :-
	http_parameters(Request,
			[ location(Path,
				   [ description('Location on this server to describe')
				   ])
			]),
	(   http_current_handler(Path, M:H, Options)
	->  reply_html_page([],
			    [ h1(['HTTP location ', Path]),
			      \handler(Request, Path, M:H, Options)
			    ])
	;   reply_html_page([],
			    [ h4(['No handler for ', Path])
			    ])
	).

handler(_Request, Path, _:http_redirect(How, Where), _Options) --> !,
	{   Where = location_by_id(Id)
	->  http_location_by_id(Id, URL)
	;   http_absolute_location(Where, URL, [relative_to(Path)])
	},
	html(p([ 'Location redirects (using "', i(\status(How)), '") to ',
		 a([href('javascript:helpHTTP("'+URL+'")')], URL),
		 '.'
	       ])).
handler(_Request, Path, _:http_reply_file(File, Options), _Options) --> !,
	file_handler(File, Path, Options).
handler(Request, Path, Closure, Options) -->
	{ extend_closure(Closure, [_], Closure1),
	  extracted_parameters(Closure1, Params)
	},
	html(h4('Implementation')),
	predicate_help(Request, Closure1),
	html(h4('Test this API')),
	api_tester(Path, Params),
	html(h4('Parameters for this API')),
	parameter_table(Params),
	dispatch_options(Options, Path).

file_handler(Spec, Location, Options) -->
	{ (   absolute_file_name(Spec, Path,
				 [ access(read),
				   file_errors(fail)
				 ])
	  ->  true
	  ;   Path = '<not found>'
	  ),
	  term_to_atom(Spec, SpecAtom),
	  default_options([cache(true)], Options, Options1)
	},
	html([ p([ 'Location serves a plain file' ]),
	       table(class(file_handler),
		     [ tr([th('File:'), td(a(href(Location), Path))]),
		       tr([th('Symbolic:'), td(SpecAtom)])
		     | \file_options(Options1)
		     ])
	     ]).

default_options([], Options, Options).
default_options([H|T], Options0, Options) :-
	functor(H, Name, 1),
	functor(Gen, Name, 1),
	(   option(Gen, Options0)
	->  default_options(T, Options0, Options)
	;   default_options(T, [H|Options0], Options)
	).

file_options([]) --> [].
file_options([H|T]) -->
	file_option(H),
	file_options(T).

file_option(Name=Value) --> !,
	{ Term =.. [Name, Value] },
	file_option(Term).
file_option(cache(true)) --> !,
	html(tr([ th('Cache:'),
		  td(['Supports ', code('If-modified-since')])
		])).
file_option(mime_type(Type)) --> !,
	html(tr([ th('Mime-type'), td(Type) ])).
file_option(_) -->
	[].

%%	status(+How)//
%
%	Emit HTTP code and comment for status.
%
%	@tbd	Use a clean interface from http_header.

status(How) -->
	{ http_header:status_number(How, Code),
	  phrase(http_header:status_comment(How), CommentCodes),
	  atom_codes(Comment, CommentCodes)
	},
	html([Code, Comment]).


%%	predicate_help(+Request, +Closure)// is det.
%
%	Provide the help-page of the implementing predicate.

predicate_help(Request, Closure) -->
	{ resolve_location(Closure, Closure1),
	  closure_pi(Closure1, PI),
	  edit_options(Request, Options)
	},
	object_page(PI,
		    [ header(false)
		    | Options
		    ]), !.
predicate_help(_Request, Closure) -->
	{ closure_pi(Closure, PI) },
	html(p('The implementing predicate ~q is not documented'-[PI])).

resolve_location(Closure, M:G) :-
	predicate_property(Closure, imported_from(M)), !,
	strip_module(Closure, _, G).
resolve_location(Closure, Closure).


%%	edit_options(+Request, -Options) is det.
%
%	Assume we can show and edit option if we are allowed to access
%	the HTTP location pldoc(edit).

edit_options(Request, [edit(true)]) :-
	catch(http:authenticate(pldoc(edit), Request, _), _, fail), !.
edit_options(_, []).


%%	dispatch_options(+Options, +Path)// is det.
%
%	Describe the dispatching options

dispatch_options([], _) -->
	[].
dispatch_options(List, Path) -->
	html([ h4('Notes'),
	       ul(class(http_options),
		  \dispatch_items(List, Path))
	     ]).

dispatch_items([], _) --> [].
dispatch_items([H|T], Path) -->
	dispatch_item(H, Path),
	dispatch_items(T, Path).


dispatch_item(prefix(true), Path) --> !,
	html(li(['Handler processes all paths that start with ', code(Path)])).
dispatch_item(Option, _) -->
	dispatch_item(Option), !.

dispatch_item(authentication(_)) --> !,
	html(li('Request requires authentication')).
dispatch_item(time_limit(Limit)) --> !,
	(   { number(Limit) }
	->  html(li('Server limits processing time to ~w seconds'-[Limit]))
	;   []
	).
dispatch_item(chunked) --> !,
	html(li('Reply uses HTTP chunked encoding if possible')).
dispatch_item(spawn(On)) --> !,
	(    {atom(On)}
	->  html(li(['Requests are spawned on pool "', i(On), '"']))
	;   html(li('Requests are spawned on a new thread'))
	).
dispatch_item(_) -->
	[].


%%	parameter_table(+Params)// is det.
%
%	Provide help on the parameters

parameter_table([]) --> !,
	html(p(class(http_parameters),
	       'Request does not handle parameters')).
parameter_table(Params) -->
	html([ table(class(http_parameters),
		     [ tr([th('Name'), th('Type'), th('Default'), th('Description')])
		     | \parameters(Params, 1)
		     ])
	     ]).

parameters([], _) --> [].
parameters([group(Members, Options)|T], _N) --> !,
	html(tr(class(group),
		[ th(colspan(4), \group_title(Options))
		])),
	parameters(Members, 0),
					% typically, this should be
					% a group again
	parameters(T, 0).
parameters([H|T], N) -->
	{ N1 is N + 1,
	  (   N mod 2 =:= 0
	  ->  Class = even
	  ;   Class = odd
	  )
	},
	parameter(H, Class),
	parameters(T, N1).

parameter(param(Name, Options), Class) -->
	html(tr(class(Class),
		[ td(class(name), Name),
		  td(\param_type(Options)),
		  td(\param_default(Options)),
		  td(\param_description(Options))
		])).

group_title(Options) -->
	{ option(description(Title), Options)
	}, !,
	html(Title).
group_title(Options) -->
	{ option(generated(Pred), Options), !,
	  (   doc_comment(Pred, _Pos, Summary0, _Comment)
	  ->  (   atom_concat(Summary, '.', Summary0)
	      ->  true
	      ;	  Summary = Summary0
	      )
	  ;   format(string(Summary), 'Parameter group generated by ~q', [Pred])
	  )
	},
	html(Summary).
group_title(_) -->
	html('Parameter group').

%%	param_type(+Options)// is det.
%
%	Emit a description of the type in HTML.

param_type(Options) -->
	{ select(list(Type), Options, Rest) }, !,
	param_type([Type|Rest]).
param_type(Options) -->
	{ type_term(Type),
	  memberchk(Type, Options), !
	},
	type(Type).
param_type(_) -->
	html(string).

type((T1;T2)) --> !,
	type(T1),
	breaking_bar,
	type(T2).
type(between(L,H)) --> !,
	html('number in [~w..~w]'-[L,H]).
type(oneof(Set)) --> !,
	html(code(\set(Set))).
type(length > N) --> !,
	html('string(>~w chars)'-[N]).
type(length >= N) --> !,
	html('string(>=~w chars)'-[N]).
type(length > N) --> !,
	html('string(<~w chars)'-[N]).
type(length =< N) --> !,
	html('string(=<~w chars)'-[N]).
type(nonneg) --> !,
	html('integer in [0..)').
type(uri) --> !,
	html(['URI', \breaking_bar, 'NS:Local']).
type(X) -->
	{ term_to_atom(X, A) },
	html(A).

set([]) --> [].
set([H|T]) -->
	html(H),
	(   { T == [] }
	->  []
	;   breaking_bar,
	    set(T)
	).

%%	breaking_bar// is det.
%
%	Emits | followed by a  zero-width   white-space  that allows the
%	browser to insert a linebreak here.

breaking_bar -->
	html(['|', &('#8203')]).

%%	type_term(-Term) is nondet.
%
%	Enumerate the option-terms that are interpreted as types.
%
%	@tbd	provide a public interface from http_parameters.pl

type_term(Term) :-
	clause(http_parameters:check_type3(Term, _, _), _),
	nonvar(Term).
type_term(Term) :-
	clause(http:convert_parameter(Term, _, _), _).
type_term(Term) :-
	clause(http_parameters:check_type2(Term, _), _),
	nonvar(Term).

param_default(Options) -->
	{ memberchk(default(Value), Options), !
	},
	html(code('~w'-[Value])).
param_default(Options) -->
	{ option(optional(true), Options) }, !,
	html(i(optional)).
param_default(Options) -->
	{ memberchk(zero_or_more, Options)
	; memberchk(list(_Type), Options)
	}, !,
	html(i(multiple)).
param_default(_Options) -->
	html(i(required)).

param_description(Options) -->
	{ option(description(Text), Options) }, !,
	html(Text).
param_description(_) --> [].


%%	extracted_parameters(+Closure, -Declarations)
%
%	Return a completely  qualified  list   of  parameters  that  are
%	retrieved by calling Closure.

extracted_parameters(Closure, Declarations) :-
	calls(Closure, 5, Goals),
	closure_last_arg(Closure, Request),
	phrase(param_decls(Goals, Request), Declarations0),
	list_to_set(Declarations0, Declarations).

param_decls([], _) -->
	[].
param_decls([H|T], Request) -->
	param_decl(H, Request),
	param_decls(T, Request).

param_decl(Var, _) -->
	{ var(Var) }, !.
param_decl(M:http_parameters(Rq, Decls), Request) --> !,
	param_decl(M:http_parameters(Rq, Decls, []), Request).
param_decl(M:http_parameters(Rq, Decls, Options), Request) -->
	{ ignore(Rq == Request), !,
	  decl_goal(Options, M, Decl)
	},
	params(Decls, Decl).
param_decl(_, _) -->
	[].

decl_goal(Options, M, Module:Goal) :-
	option(attribute_declarations(G), Options), !,
	strip_module(M:G, Module, Goal).
decl_goal(_, _, -).

:- meta_predicate
	params(+, 2, ?, ?),
	param(+, 2, ?, ?).

params(V, _) -->
	{ var(V) }, !.
params([], _) -->
	[].
params([H|T], Decl) -->
	param(H, Decl),
	params(T, Decl).

param(Term, _) -->
	{ \+ compound(Term) }, !.
param(group(Params0, Options), Decl) --> !,
	{ phrase(params(Params0, Decl), GroupedParams) },
	[ group(GroupedParams, Options) ].
param(Term, _) -->
	{ Term =.. [Name, _Value, Options] }, !,
	[ param(Name, Options) ].
param(Term, Decl) -->
	{ Term =.. [Name, _Value],
	  catch(call(Decl, Name, Options), _, fail), !
	},
	[ param(Name, Options) ].
param(_, _) -->
	[].

		 /*******************************
		 *	  CLOSURE LOGIC		*
		 *******************************/

%%	extend_closure(:In, +Extra, -Out) is det.
%
%	Extend a possibly qualified closure with arguments from Extra.

extend_closure(Var, _, _) :-
	var(Var), !, fail.
extend_closure(M:C0, Extra, M:C) :- !,
	extend_closure(C0, Extra, C).
extend_closure(C0, Extra, C) :-
	C0 =.. L0,
	append(L0, Extra, L),
	C =.. L.

closure_pi(M:C, M:Name/Arity) :- !,
	functor(C, Name, Arity).
closure_pi(C, Name/Arity) :-
	functor(C, Name, Arity).

closure_last_arg(C, _) :-
	var(C), !,
	instantiation_error(C).
closure_last_arg(_:C, Last) :- !,
	closure_last_arg(C, Last).
closure_last_arg(C, Last) :-
	functor(C, _, Arity),
	arg(Arity, C, Last).


		 /*******************************
		 *	CALL-TREE ANALYSIS	*
		 *******************************/

%%	calls(:Goal, +MaxDepth, -Called) is det.
%
%	Called is the list of goals called by Goal obtained by unfolding
%	the call-tree upto the given MaxDepth.
%
%	@tbd	Without MaxDepth not all programs terminate.  Why?

:- meta_predicate
	calls(:, +, -).

calls(M:Goal, Depth, SubGoals) :-
	phrase(calls(Goal, M, Depth, SubGoals0), SubGoals0), !,
	maplist(unqualify, SubGoals0, SubGoals).

unqualify(Var, Var) :-
	var(Var), !.
unqualify(S:G, G) :-
	S == system, !.
unqualify(S:G, G) :-
	predicate_property(S:G, imported_from(system)), !.
unqualify(G, G).

calls(_, _, 0, _) --> !.
calls(Var, _, _, _) -->
	{ var(Var), ! },
	[ Var ].
calls(Goal, M, _, Done) -->
	{ seen_goal(M:Goal, Done) }, !.
calls(M:G, _, D, Done) --> !,
	calls(G, M, D, Done).
calls(Control, M, Depth, Done) -->
	{ control(Control, Members)
	}, !,
	bodies(Members, M, Depth, Done).
calls(Goal, M, _, _) -->
	{ evaluate_now(M:Goal), !,
	  ignore(catch(M:Goal, _, fail))
	},
	[].
calls(Goal, M, _, _) -->
	{ primitive(M:Goal) }, !,
	[ M:Goal ].
calls(Goal, M, Depth, Done) -->
	{ term_variables(Goal, Vars),
	  Key =.. [v|Vars],
	  '$define_predicate'(M:Goal),	% auto-import if needed
	  def_module(M:Goal, DefM),
	  qualify_goal(DefM:Goal, M, QGoal),
	  catch(findall(Key-Body, clause(QGoal, Body), Pairs), _, fail),
	  SubDepth is Depth - 1
	},
	[ M:Goal ],
	vars_bodies(Pairs, DefM, SubDepth, Done),
	{ bind_vars(Key, Pairs) }.

def_module(Callable, M) :-
	predicate_property(Callable, imported_from(M)), !.
def_module(Callable, M) :-
	strip_module(Callable, M, _).

qualify_goal(M:G, Ctx, M:QG) :-
	predicate_property(G, meta_predicate(Meta)), !,
	functor(Meta, Name, Arity),
	functor(G, Name, Arity),
	functor(QG, Name, Arity),
	qualify_args(1, Arity, Ctx, Meta, G, QG).
qualify_goal(G, _, G).

qualify_args(I, Arity, Ctx, Meta, G, QG) :-
	I =< Arity, !,
	arg(I, Meta, MA),
	arg(I, G, GA),
	(   ismeta(MA),
	    \+ isqual(GA)
	->  arg(I, QG, Ctx:GA)
	;   arg(I, QG, GA)
	),
	I2 is I+1,
	qualify_args(I2, Arity, Ctx, Meta, G, QG).
qualify_args(_, _, _, _, _, _).

ismeta(:).
ismeta(I) :- integer(I).

isqual(M:_) :-
	atom(M).

vars_bodies([], _, _, _) --> [].
vars_bodies([_-Body|T], M, Depth, Done) -->
	calls(Body, M, Depth, Done),
	vars_bodies(T, M, Depth, Done).

bodies([], _, _, _) --> [].
bodies([H|T], M, Depth, Done) -->
	calls(H, M, Depth, Done),
	bodies(T, M, Depth, Done).

%%	bind_vars(+Key, +Pairs) is det.
%
%	Pairs  contains  the  variable  bindings    after  scanning  the
%	alternative computation paths. Key are   the  initial variables.
%
%	@tbd What we should do  is  find   all  bindings  for a specific
%	variable, compute the most specific   generalization of this set
%	and unify it with the variable in Key.   For now, we only try to
%	unify all of them with the  input variable. That deals correctly
%	with the case  where  no  path   binds  the  variable  (this  is
%	typically the case for input variables   and that is our biggest
%	concern at the moment).

bind_vars(Key, Pairs) :-
	functor(Key, _, Arity),
	bind_vars(1, Arity, Key, Pairs).

bind_vars(I, Arity, Key, Pairs) :-
	I =< Arity, !,
	arg(I, Key, V),
	maplist(pair_arg(I), Pairs, Vars),
	ignore(maplist(=(V), Vars)).
bind_vars(_, _, _, _).

pair_arg(I, Key-_, V) :-
	arg(I, Key, V).

control((A,B), [A,B]).
control((A;B), [A,B]).
control((A->B), [A,B]).
control((A*->B), [A,B]).
control(call(G, A1), [Goal]) :-
	extend_closure(G, [A1], Goal).
control(call(G, A1, A2), [Goal]) :-
	extend_closure(G, [A1, A2], Goal).
control(call(G, A1, A2, A3), [Goal]) :-
	extend_closure(G, [A1, A2, A3], Goal).
control(call(G, A1, A2, A3, A4), [Goal]) :-
	extend_closure(G, [A1, A2, A3, A4], Goal).

primitive(_:Goal) :-
	functor(Goal, Name, Arity),
	current_predicate(system:Name/Arity), !.
primitive(Goal) :-
	\+ predicate_property(Goal, interpreted).

seen_goal(Goal, Done) :-
	member_open_list(X, Done),
	variant(X, Goal), !.

member_open_list(_, List) :-
	var(List), !, fail.
member_open_list(X, [X|_]).
member_open_list(X, [_|T]) :-
	member_open_list(X, T).

%%	evaluate_now(:Goal) is semidet.
%
%	If =true=, call Goal and  propagate   bindings  that it produces
%	instead of unfolding its call-tree. This  was introduced to deal
%	with  extracted_parameters/2,  which    dynamically   constructs
%	option-lists for http_parameters/3.
%
%	@see The hook evaluate/1 extends the definition

%%	evaluate(:Goal) is semidet.
%
%	Multifile hook to  extend  the  goals   that  are  evaluated  by
%	evaluate_now/1.

:- multifile
	evaluate/1.

evaluate_now(Var) :-
	var(Var), !, fail.
evaluate_now(Goal) :-
	evaluate(Goal), !.
evaluate_now(_:Goal) :-
	evaluate_now(Goal).
evaluate_now(_ = _).
evaluate_now(_ is _).
evaluate_now(append(L1,L2,_)) :-
	is_list(L1),
	is_list(L2).
evaluate_now(append(L1,_)) :-
	is_list(L1),
	maplist(is_list, L1).


		 /*******************************
		 *	   AUTOCOMPLETE		*
		 *******************************/

max_results_displayed(50).

quick_find_div_content -->
	html([ span(id(qf_label), 'Quick find:'),
	       \autocomplete_finder,
	       input([ value('Show'), type(submit),
		       onClick('showLocation();')
		     ]),
	       script(type('text/javascript'),
		      [ 'function showLocation()\n',
			'{ helpHTTP(document.getElementById("ac_location_input").value);\n',
			'}'
		      ])
	     ]).

autocomplete_finder -->
	{ max_results_displayed(Max)
	},
	autocomplete(ac_location,
		     [ query_delay(0.2),
		       auto_highlight(false),
		       max_results_displayed(Max),
		       width('40ex')
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
			  fields:["label","location"]
			};\n',
'  oDS.maxCacheEntries = 5;\n',
'  var oAC = new YAHOO.widget.AutoComplete("~w", "~w", oDS);\n'-[Input, Container],
'  oAC.resultTypeList = false;\n',
'  oAC.formatResult = function(oResultData, sQuery, sResultMatch) {
     var into = "<span class=\\"acmatch\\">"+sQuery+"</span>";
     var sLabel = oResultData.label.replace(sQuery, into);
     return sLabel;
   };\n',
'  oAC.itemSelectEvent.subscribe(function(sType, aArgs) {
     var oData = aArgs[2];
     helpHTTP(oData.location);
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

%%	ac_location(+Request)
%
%	HTTP handler to for autocompletion on HTTP handlers.

ac_location(Request) :-
	max_results_displayed(DefMax),
	http_parameters(Request,
			[ query(Query, [ description('String to find in HTTP path') ]),
			  maxResultsDisplayed(Max,
					      [ integer, default(DefMax),
						description('Max number of results returned')
					      ])
			]),
	autocompletions(Query, Max, Count, Completions),
	reply_json(json([ query = json([ count=Count
				       ]),
			  results = Completions
			])).

autocompletions(Query, Max, Count, Completions) :-
	findall(C, ac_object(Query, C), Completions0),
	sort(Completions0, Completions1),
	length(Completions1, Count),
	first_n(Max, Completions1, Completions2),
	maplist(obj_result, Completions2, Completions).

obj_result(Location, json([ label=Location,
			    location=Location
			  ])).

first_n(0, _, []) :- !.
first_n(_, [], []) :- !.
first_n(N, [H|T0], [H|T]) :-
	N2 is N - 1,
	first_n(N2, T0, T).

ac_object(Query, Location) :-
	http_current_handler(Location, _:_Handler, _Options),
	sub_atom(Location, _, _, _, Query).
