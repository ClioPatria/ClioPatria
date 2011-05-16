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

:- module(cpa_wiki,
	  [ serve_page/2
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_dirindex)).
:- use_module(library(http/html_write)).
:- use_module(library(pldoc/doc_wiki)).
:- use_module(library(readutil)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(library(pldoc/doc_index)).
:- use_module(library(pldoc/doc_html),
	      except([ file//2,
		       include//3
		     ])).

/** <module> ClioPatria wiki-page server

This module serves wiki-pages from (by default) cliopatria(web/help). If
the user requests an X.html page, it runs SWI-Prolog's PlDoc wiki-engine
over the associated X.txt file.

@see The file load.pl binds this functionality to cliopatria(web/help).
*/

:- setting(http:index_files,
	   list(atom),
	   [ 'index.txt', 'index.html' ],
	   'List of files that provide a directory index').

%
%	serve_page(+Alias, +Request)
%
%	HTTP handler for files below the file-path Alias. .txt files are
%	served as Wiki-pages. All other files   are  served according to
%	the rules of http_reply_file/3. To serve   a directory, one must
%	create a file search path for it   and decide on the location in
%	the web-hierarchy. Here is an example that serves files from the
%	subdirectory =www= below  the  search-path   =myapp=  from  HTTP
%	locations below =|/web|=.
%
%	    ==
%	    user:file_search_path(web_files, myapp(www)).
%
%	    :- http_handler(root(web), serve_page(web_files), [prefix]).
%	    ==

serve_page(Alias, Request) :-
	memberchk(path_info(Relative), Request),
	Spec =.. [ Alias, Relative ],
	http_safe_file(Spec, []),
	find_file(Spec, File), !,
	setup_call_cleanup(b_setval(doc_alias, Alias),
			   serve_file(File, Request),
			   nb_delete(Alias)).
serve_page(Alias, Request) :-
	\+ memberchk(path_info(_), Request), !,
	serve_page(Alias, [path_info('index.html')|Request]).
serve_page(_, Request) :-
	http_404([], Request).

%%	find_file(+Spec, -File) is semidet.
%
%	Translate Spec into a File  in   the  document-root tree. If the
%	given extension is .html, also look for   .txt files that can be
%	translated into HTML.

find_file(Spec, File) :-
	spec_replace_extension(Spec, html, txt, TxtSpec),
	absolute_file_name(TxtSpec,
			   File,
			   [ access(read),
			     file_errors(fail)
			   ]), !.
find_file(Spec, File) :-
	absolute_file_name(Spec,
			   File,
			   [ access(read),
			     file_errors(fail)
			   ]).
find_file(Spec, File) :-
	absolute_file_name(Spec,
			   File,
			   [ access(read),
			     file_errors(fail),
			     file_type(directory)
			   ]).

spec_replace_extension(File0, Ext0, Ext, File) :-
	atomic(File0), !,
	file_name_extension(Base, Ext0, File0),
	file_name_extension(Base, Ext, File).
spec_replace_extension(Comp0, Ext0, Ext, Comp) :-
	Comp0 =.. [Alias,Inside0],
	spec_replace_extension(Inside0, Ext0, Ext, Inside),
	Comp =.. [Alias,Inside].

%%	serve_file(+File, +Request) is det.
%%	serve_file(+Extension, +File, +Request) is det.
%
%	Serve the requested file.

serve_file(File, Request) :-
	file_name_extension(_, Ext, File),
	debug(plweb, 'Serving ~q; ext=~q', [File, Ext]),
	serve_file(Ext, File, Request).

serve_file('',  Dir, Request) :-
	exists_directory(Dir), !,
	(   sub_atom(Dir, _, _, 0, /),
	    serve_index_file(Dir, Request)
	->  true
	;   http_reply_dirindex(Dir, [unsafe(true)], Request)
	).
serve_file(txt, File, Request) :-
	http_parameters(Request,
			[ format(Format, [ oneof([raw,html]),
					   default(html)
					 ])
			]),
	Format == html, !,
	read_file_to_codes(File, String, []),
	setup_call_cleanup(b_setval(pldoc_file, File),
			   serve_wiki(String, File, Request),
			   nb_delete(pldoc_file)).
serve_file(_Ext, File, Request) :-	% serve plain files
	http_reply_file(File, [unsafe(true)], Request).

%%	serve_index_file(+Dir, +Request) is semidet.
%
%	Serve index.txt or index.html, etc. if it exists.

serve_index_file(Dir, Request) :-
        setting(http:index_files, Indices),
        member(Index, Indices),
	ensure_slash(Dir, DirSlash),
	atom_concat(DirSlash, Index, File),
        access_file(File, read), !,
        serve_file(File, Request).

ensure_slash(Dir, Dir) :-
	sub_atom(Dir, _, _, 0, /), !.
ensure_slash(Dir0, Dir) :-
	atom_concat(Dir0, /, Dir).


%%	serve_wiki(+String, +File, +Request) is det.
%
%	Emit page from wiki content in String.

serve_wiki(String, File, Request) :-
	wiki_codes_to_dom(String, [], DOM0),
	(   sub_term(h1(_, Title), DOM0)
	->  true
	;   Title = 'SWI-Prolog'
	),
	insert_edit_button(DOM0, File, Request, DOM),
	setup_call_cleanup(b_setval(pldoc_options,
				    [ prefer(manual)
				    ]),
			   serve_wiki_page(Title, DOM),
			   nb_delete(pldoc_options)).

serve_wiki_page(Title, DOM) :-
	reply_html_page(pldoc(wiki),
			[ title(Title)
			],
			DOM).

insert_edit_button(DOM, _, Request, DOM) :-
	\+ catch(http:authenticate(pldoc(edit), Request, _), _, fail), !.
insert_edit_button([h1(Attrs,Title)|DOM], File, _,
		   [h1(Attrs,[ span(style('float:right'),
				   \edit_button(File, [edit(true)]))
			     | Title
			     ])|DOM]) :- !.
insert_edit_button(DOM, File, _,
		   [ h1(class(wiki),
			[ span(style('float:right'),
			       \edit_button(File, [edit(true)]))
			])
		   | DOM
		   ]).


:- public				% Called through wiki \Term
	include//3,
	file//2.

		 /*******************************
		 *	     RENDERING		*
		 *******************************/

%%	include(+Object, +Type, +Options)//

include(Object, Type, Options) -->
	pldoc_html:include(Object, Type,
			   [ map_extension([txt-html])
			   | Options
			   ]).

%%	file(+Path, Options)//
%
%	Trap translation of \file(+Path,  Options).   The  first  clause
%	reduces the label of the file to the plain file-name if the file
%	is inside the help-system.

file(Path, Options) -->
	{ \+ option(label(_), Options),
	  file_name_extension(Base, txt, Path),
	  option(absolute_path(AbsPath), Options),
	  current_alias_root(DocRoot),
	  sub_atom(AbsPath, 0, _, _, DocRoot), !,
	  file_base_name(Base, Label),
	  file_href(Options, Options1)
	},
	pldoc_html:file(Path,
			[ label(Label),
			  map_extension([txt-html])
			| Options1
			]).
file(File, Options) -->
	{ file_href(Options, Options1)
	},
	pldoc_html:file(File,
			[ map_extension([txt-html])
			| Options1
			]).


file_href(Options0, Options) :-
	\+ ( nb_current(pldoc_file, CFile),
	     CFile \== []
	   ),
	option(absolute_path(Path), Options0),
	current_alias_root(DocRoot),
	atom_concat(DocRoot, DocLocal, Path), !,
	ensure_leading_slash(DocLocal, HREF),
	Options = [ href(HREF) | Options0 ].
file_href(Options0, Options) :-
	nb_current(pldoc_file, CFile),
	CFile \== [],
	option(absolute_path(Path), Options0),
	plfile_href(Path, HREF),
	Options = [ href(HREF)|Options0 ].
file_href(Options, Options).

%%	plfile_href(+Path, -HREF) is det.
%
%	Create a link for a file to see  the (pretty) source if the file
%	is inside the help system. Otherwise create a normal PlDoc link.

plfile_href(Path, HREF) :-
	file_name_extension(_, Ext, Path),
	prolog_file_type(Ext, prolog),
	current_alias_root(DocRoot),
	sub_atom(Path, 0, _, _, DocRoot), !,
	doc_file_href(Path, HREF0),
	atom_concat(HREF0, '?show=src', HREF).
plfile_href(Path, HREF) :-
	doc_file_href(Path, HREF).

%%	current_alias_root(-Root)
%
%	Root is the root of the current file-alias we are served from.

current_alias_root(DocRoot) :-
	(   nb_current(doc_alias, Val), Val \== []
	->  Alias = Val
	;   Alias = document_root
	),
	Term =.. [Alias,'.'],
	absolute_file_name(Term,
			   DocRoot,
			   [ file_type(directory),
			     access(read)
			   ]).


ensure_leading_slash(Path, SlashPath) :-
	(   sub_atom(Path, 0, _, _, /)
	->  SlashPath = Path
	;   atom_concat(/, Path, SlashPath)
	).
