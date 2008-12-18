/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2006, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(serql_server,
	  [ serql_server/0,
	    serql_server/1,		% +Options
	    serql_welcome/0
	  ]).

/** <module> SeRQL main module

This module loads the SWI-Prolog SeRQL   server  as a library, providing
the public predicates defined in the   header. Before loading this file,
the user should set up a the search path `serql'.  For example:

==
:- dynamic
	user:file_search_path/2.
:- multifile
	user:file_search_path/2.

user:file_search_path(serql, '/usr/local/serql').

:- use_module(serql(load)).
==

*/

:- dynamic
	user:file_search_path/2.
:- multifile
	user:file_search_path/2.

:- (   user:file_search_path(serql, _)
   ->  true
   ;   prolog_load_context(directory, Dir),
       assert(user:file_search_path(serql, Dir))
   ).

user:file_search_path(triple20,      serql('Triple20/src')).
user:file_search_path(library,	     serql(lib)).
user:file_search_path(ontology_root, serql('Ontologies')).

:- load_files([version], [silent(true), if(not_loaded)]).
:- check_prolog_version(50661).		% Demand >= 5.6.61

:- load_files([ parms,
		library(option),
		library(debug),
		library(lists),
		library(settings),
		library(error),
		library(broadcast),
		server,
		library(semweb/rdf_db),
		library(semweb/rdf_persistency),
		library(semweb/rdf_portray),
		library(http/http_session),
		library(http/http_dispatch),
		library(http/thread_httpd),
		user_db,
		openid,
		rdf_store,
		jrn_export
	      ],
	      [ %silent(true),
		if(not_loaded)
	      ]).

%%	serql_server is det.
%%	serql_server(+Options) is det.
%	
%	Start the HTTP server.  Defined options are:
%	
%		* port(Port)
%		Attach to Port instead of the port specified in the
%		configuration file.
%		
%		* after_load(+Goal)
%		Run Goal after loading/initialising the database.  Note
%		that serql_server/1 is not a meta-predicate.  Goal is
%		called in =user= and must be prefixed if calling in
%		another module is required.
%		
% 	In addition, all settings as defined in parms.pl are allowed as
% 	options:
%
%	==
%	?- serql_server([max_idle_time(600)]).
%	==

serql_server :-
	serql_server([]).

serql_server(Options) :-
	load_settings('settings.db'),
	after_load_option(Options, AfterLoad, Options1),
	set_options(Options1, Options2),
	setting(http:port, Port),
	attach_account_info,
	set_session_options,
	setting(http:workers, Workers),
	setting(http:worker_options, Settings),
	merge_options([workers(Workers)|Options2], Settings, HTTPOptions),
	serql_server(Port, HTTPOptions),
	print_message(informational, serql(server_started(Port))),
	serql_server_set_property(loading(true)),
	rdf_setup_store(Options2),
	user:AfterLoad,
	serql_server_set_property(loading(false)).


after_load_option(Options0, AfterLoad, Options) :-
	select(after_load(AfterLoad), Options0, Options), !.
after_load_option(Options, true, Options).


%%	set_options(+List, -NotProcesses)
%	
%	Set parameters as defined in  parms.pl   from  all  options that
%	match a setting name.  Using +Option(Value, ...), multiple rules
%	are appended.

set_options([], []).
set_options([+Opt|T0], T) :- !,
	Opt =.. [Name, Value],
	option_module(OptM),
	current_setting(OptM:Name), !,
	setting(OptM:Name, Old),
	must_be(list(any), Old),
	(   memberchk(Value, Old)
	->  true
	;   set_setting(OptM:Name, [Value|Old])
	),
	set_options(T0, T).
set_options([Opt|T0], T) :-
	Opt =.. [Name, Value],
	option_module(OptM),
	current_setting(OptM:Name), !,
	set_setting(OptM:Name, Value),
	set_options(T0, T).
set_options([H|T0], [H|T]) :-
	set_options(T0, T).

option_module(serql_parms).
option_module(http).


attach_account_info :-
	setting(serql_parms:user_data, File),
	set_user_database(File).

%%	set_session_options
%
%	Initialise session timeout from =|http:max_idle_time|=.

set_session_options :-
	setting(http:max_idle_time, Idle),
	http_set_session_options([timeout(Idle)]).


		 /*******************************
		 *	 UPDATE SETTINGS	*
		 *******************************/

update_workers(New) :-
	setting(http:port, Port),
	http_current_worker(Port, _),
	http_workers(Port, New).

:- listen(settings(changed(http:max_idle_time, _, New)),
	  http_set_session_options([timeout(New)])).
:- listen(settings(changed(http:workers, _, New)),
	  update_workers(New)).


		 /*******************************
		 *	      BANNER		*
		 *******************************/

%%	serql_welcome
%
%	Print welcome banner.

serql_welcome :-
	setting(http:port, Port),
	print_message(informational, serql(welcome(Port))).


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(serql(server_started(Port))) -->
	{ gethostname(Host),
	  http_location_by_id(serql_home, Home)
	},
	[ 'Started SeRQL server at port ~w'-[Port], nl,
	  'You may access the server at http://~w:~w~w'-[Host, Port, Home]
	].
prolog:message(serql(welcome(DefaultPort))) -->
	[ nl,
	  'Use one of the calls below to start the SeRQL server:', nl, nl,
	  '  ?- serql_server.               % start at port ~w'-[DefaultPort], nl,
	  '  ?- serql_server([port(Port)]). % start at Port'
	].


		 /*******************************
		 *		XREF		*
		 *******************************/

:- multifile
	prolog:called_by/2.

prolog:called_by(serql_server(Options), Goals) :-
	findall(G, (member(after_load(G), Options), callable(G)), Goals).
