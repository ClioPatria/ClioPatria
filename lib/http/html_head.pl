/*  This file is part of ClioPatria.

    Author:	Jan Wielemaker <wielemak@science.uva.nl>
    HTTP:	http://e-culture.multimedian.nl/
    GITWEB:	http://gollem.science.uva.nl/git/ClioPatria.git
    GIT:	git://gollem.science.uva.nl/home/git/ClioPatria.git
    GIT:	http://gollem.science.uva.nl/home/git/ClioPatria.git
    Copyright:  2007, E-Culture/MultimediaN

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

:- module(html_head,
	  [ html_resource/2,		% +Resource, +Attributes
	    html_requires//1,		% +Resource
	    absolute_http_location/2	% +Spec, -Path
	  ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/mimetype)).
:- use_module(library(error)).
:- use_module(library(settings)).
:- use_module(library(lists)).
:- use_module(library(option)).

/** <module> Deal with CSS and scripts

This module is to clean up the   mess related to managing Javascript and
CSS files. It defines relations between scripts and style files.

@tbd	Process aggregate declarations
@tbd	Cache some of the computations
*/

:- dynamic
	html_resource/3.		% Resource, Source, Properties

%%	html_resource(+About, +Properties) is det.
%
%	Register an HTML head resource.  About   is  either an atom that
%	specifies an HTTP location or  a   term  Alias(Sub).  This works
%	similar to absolute_file_name/2.  See   http:location_path/2  for
%	details.  Recognised properties are:
%	
%		* requires(+Requirements)
%		Other required script and css files.  If this is a plain
%		file name, it is interpreted relative to the declared
%		resource.  Requirements can be a list, which is equivalent
%		to multiple requires properties.
%		
%		* virtual(+Bool)
%		If =true= (default =false=), do not include About itself,
%		but only its dependencies.  This allows for defining an
%		alias for one or more resources.
%		
%		* aggregate(+List)
%		States that About is an aggregate of the resources in
%		List.

html_resource(About, Properties) :-
	source_location(File, Line), !,
	retractall(html_resource(About, File:Line, _)),
	assert(html_resource(About, File:Line, Properties)).
html_resource(About, Properties) :-
	assert(html_resource(About, -, Properties)).


%%	html_requires(+ResourceOrList)// is det.
%
%	Include ResourceOrList and all dependencies derived from it
%	and add them to the HTML =head= using html_post/2.

html_requires(Required) -->
	{ is_list(Required), !,
	  requirements(Required, Paths)
	},
	html_include(Paths).
html_requires(Required) -->
	{ requirements([Required], Paths)
	},
	html_include(Paths).
	
requirements(Required, Paths) :-
	phrase(requires(Required), List),
	sort(List, Paths).		% Can order matter?

%%	requires(+Spec)// is det.
%%	requires(+Spec, +Base)// is det.
%
%	True if Files is the set of  files   that  need to be loaded for
%	Spec. Note that Spec normally appears in  Files, but this is not
%	necessary (i.e. virtual resources  or   the  usage  of aggregate
%	resources).

requires(Spec) -->
	requires(Spec, /).

requires([], _) -->
	[].
requires([H|T], Base) --> !,
	requires(H, Base),
	requires(T, Base).
requires(Spec, Base) -->
	{ res_properties(Spec, Properties),
	  absolute_http_location(Spec, Base, File)
	},
	(   { option(virtual(true), Properties) }
	->  []
	;   [File]
	),
	requires_from_properties(Properties, File).

requires_from_properties([], _) --> 
	[].
requires_from_properties([H|T], Base) -->
	requires_from_property(H, Base),
	requires_from_properties(T, Base).

requires_from_property(requires(What), Base) --> !,
	requires(What, Base).
requires_from_property(_, _) -->
	[].


%%	res_properties(+Spec, -Properties) is det.
%
%	True if Properties is the set of defined properties on Spec.

res_properties(Spec, Properties) :-
	findall(P, res_property(Spec, P), Properties0),
	list_to_set(Properties0, Properties).

res_property(Spec, Property) :-
	html_resource(About, _, Properties),
	same_resource(Spec, About),
	member(Property, Properties).

%%	same_resource(+R1, +R2) is semidet.
%
%	True if R1 an R2 represent the same resource.  R1 and R2 are
%	resource specifications are defined by absolute_http_location/2.

same_resource(R, R) :- !.
same_resource(R1, R2) :- 
	resource_base_name(R1, B),
	resource_base_name(R2, B),
	absolute_http_location(R1, Path),
	absolute_http_location(R2, Path).

resource_base_name(Atom, Base) :-
	atomic(Atom), !,
	file_base_name(Atom, Base).
resource_base_name(Compound, Base) :-
	arg(1, Compound, Base0),
	file_base_name(Base0, Base).

%%	html_include(+PathOrList)// is det.
%
%	Post =link= and =script=  elements  to   =head=  to  include the
%	desired CSS and Javascript heads.
	
html_include([]) --> !.
html_include([H|T]) --> !,
	html_include(H),
	html_include(T).
html_include(Path) -->
	{ file_mime_type(Path, Mime) }, !,
	html_include(Mime, Path).

html_include(text/css, Path) --> !,
	html_post(head, link([ rel(stylesheet),
			       type('text/css'),
			       href(Path)
			     ], [])).
html_include(text/javascript, Path) --> !,
	html_post(head, script([ type('text/javascript'),
				 src(Path)
			       ], [])).
html_include(Mime, Path) -->
	{ print_message(warning, html_include(dont_know, Mime, Path))
	}.



		 /*******************************
		 *	      PATHS		*
		 *******************************/

:- multifile
	http:location_path/2.
:- dynamic
	http:location_path/2.

%%	http_location_path(+Alias, -Expansion) is det.
%
%	Expansion is the expanded HTTP location for Alias. As we have no
%	condition search, we demand a single  expansion for an alias. An
%	ambiguous alias results in a printed   warning.  A lacking alias
%	results in an exception.
%	
%	@error	existence_error(http_alias, Alias)

http_location_path(Alias, Path) :-
	findall(Path, http:location_path(Alias, Path), Paths0),
	sort(Paths0, Paths),
	(   Paths == [One]
	->  Path = One
	;   Paths = [Path|_]
	->  print_message(warning, ambiguous_http_location(Alias, Paths))
	;   Alias \== prefix
	->  existence_error(http_alias, Alias)
	).
http_location_path(prefix, Path) :-
	setting(http:prefix, Path).


%%	absolute_http_location(+Spec, -Path) is det.
%%	absolute_http_location(+Spec, +Base, -Path) is det.
%
%	True if Path is the full HTTP   location  for Spec. This behaves
%	very much like absolute_file_name/2. Path-alias   are defined by
%	the dynamic multifile predicate  http:location_path/2, using the
%	same syntax as user:file_search_path/2.
%	
%	@tbd	Use caching, resetting the cache on reload and change of
%		the prefix setting.

absolute_http_location(Spec, Path) :-
	absolute_http_location(Spec, /, Path).

absolute_http_location(Spec, Base, Path) :-
	(   atomic(Spec)
	->  relative_to(Base, Spec, Path)
	;   Spec =.. [Alias, Sub],
	    http_location_path(Alias, Parent),
	    absolute_http_location(Parent, /, ParentLocation),
	    phrase(sub_list(Sub), List),
	    concat_atom(List, /, SubAtom),
	    (	ParentLocation == ''
	    ->	Path = SubAtom
	    ;	sub_atom(ParentLocation, _, _, 0, /)
	    ->	atom_concat(ParentLocation, SubAtom, Path)
	    ;	concat_atom([ParentLocation, SubAtom], /, Path)
	    )
	).

relative_to(/, Path, Path).
relative_to(_Base, Path, Path) :-
	sub_atom(Path, 0, _, _, /).
relative_to(Base, Local, Path) :-
	absolute_file_name(Local, Path,
			   [ relative_to(Base)
			   ]).

sub_list(Var) -->
	{ var(Var), !,
	  instantiation_error(Var)
	}.
sub_list(A/B) --> !,
	sub_list(A),
	sub_list(B).
sub_list(A) -->
	[A].
