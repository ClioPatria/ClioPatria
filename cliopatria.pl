/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2010, University of Amsterdam
			      VU University Amsterdam

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

:- module(cp_server,
	  [ cp_server/0,
	    cp_server/1,		% +Options
	    cp_welcome/0,
	    cp_after_load/1		% :Goal
	  ]).

/** <module> ClioPatria main module

This module loads the ClioPatria  server   as  a  library, providing the
public predicates defined in the header.   Before loading this file, the
user should set up a the search path =cliopatria=. For example:

==
:- dynamic
	user:file_search_path/2.
:- multifile
	user:file_search_path/2.

user:file_search_path(cliopatria, '/usr/local/cliopatria').

:- use_module(cliopatria(load)).
==

*/

:- dynamic
	user:file_search_path/2.
:- multifile
	user:file_search_path/2.

:- (   user:file_search_path(cliopatria, _)
   ->  true
   ;   prolog_load_context(directory, Dir),
       assert(user:file_search_path(cliopatria, Dir))
   ).

:- load_files([lib/version], [silent(true), if(not_loaded)]).
:- check_prolog_version(51106).		% Demand >= 5.11.6
:- register_git_component('ClioPatria',
			  [ home_url('http://www.swi-prolog.org/web/ClioPatria.html')
			  ]).

:- load_files([ parms,
		skin,			% HTML Page layout
		library(option),
		library(debug),
		library(lists),
		library(settings),
		library(error),
		library(broadcast),
		library(thread_pool),

		library(semweb/rdf_db),
		library(semweb/rdf_persistency),

		library(http/http_session),
		library(http/http_dispatch),
		library(http/thread_httpd),

		user(user_db),
		user(openid),
		user(preferences),

		api(sesame),
		api(journal),			% export journal information
		api(sparql),
		api(export),

		applications(admin),
		applications(user),
		applications(browse)
	      ],
	      [ silent(true),
		if(not_loaded)
	      ]).

:- dynamic
	after_load_goal/1.

%%	cp_server is det.
%%	cp_server(:Options) is det.
%
%	Start the HTTP server.  This predicate preforms the following
%	steps:
%
%	    1. Load application settings from =|settings.db|=
%	    2. Load user-data from =|users.db|=
%	    3. Start the HTTP server
%	    4. Load the RDF persistent database from =|RDF-store|=
%	    5. Execute `after load' options registered using
%	       cp_after_load/1.
%
%	Defined options are:
%
%	    * port(Port)
%	    Attach to Port instead of the port specified in the
%	    configuration file settings.db.

:- meta_predicate
	cp_server(:).

cp_server :-
	cp_server([]).

cp_server(Options) :-
	meta_options(is_meta, Options, QOptions),
	load_settings('settings.db'),
	option(after_load(AfterLoad), QOptions, true),
	attach_account_info,
	set_session_options,
	setting(http:port, DefPort),
	setting(http:workers, Workers),
	setting(http:worker_options, Settings),
	merge_options([workers(Workers)|QOptions], Settings, HTTPOptions),
	option(port(Port), QOptions, DefPort),
	http_server(http_dispatch,
		    [ port(Port),
		      HTTPOptions
		    ]),
	print_message(informational, cliopatria(server_started(Port))),
	setup_call_cleanup(http_handler(root(.), busy_loading,
					[ priority(1000),
					  hide_children(true),
					  id(busy_loading),
					  prefix
					]),
			   rdf_attach_store(QOptions, AfterLoad),
			   http_delete_handler(id(busy_loading))).

is_meta(after_load).

%%	rdf_attach_store(+Options, :AfterLoad) is det.
%
%	Attach     the     RDF     store       using     the     setting
%	cliopatria:persistent_store and call the `after-load' goals.
%
%	@see cp_after_load/1 for registering after-load goals.

rdf_attach_store(Options, AfterLoad) :-
	setting(cliopatria:persistent_store, Directory),
	Directory \== '', !,
        rdf_attach_db(Directory, Options),
	forall(after_load_goal(Goal),
	       call_warn(Goal)),
	call_warn(AfterLoad).

call_warn(Goal) :-
	(   catch(Goal, E, true)
	->  (   var(E)
	    ->	true
	    ;	print_message(warning, E)
	    )
	;   print_message(warning, goal_failed(Goal))
	).

%%	cp_after_load(:Goal) is det.
%
%	Register Goal to be executed after  reloading the RDF persistent
%	DB. Note that  already  registered   goals  are  not duplicated.
%	Running a goal after loading the   database  is commonly used to
%	ensure presence of relevant schemas or build additional indices.
%	Note that it is possible to   start  a thread for time-consuming
%	tasks (see thread_create/3).

:- meta_predicate
	after_load(0).

cp_after_load(Goal) :-
	(   after_load_goal(Goal)
	->  true
	;   assert(after_load_goal(Goal))
	).


%%	busy_loading(+Request)
%
%	This HTTP handler is  pushed  to   overrule  all  actions of the
%	server while the server is restoring   its  persistent state. It
%	replies with the 503  (unavailable)   response,  indicating  the
%	progress of restoring the repository.

:- dynamic
	loading_done/2.

busy_loading(_Request) :-
	rdf_statistics(triples(Triples)),
	(   loading_done(Nth, Total)
	->  Extra = [ '; ~D of ~D graphs.'-[Nth, Total] ]
	;   Extra = [ '.' ]
	),
	HTML = p([ 'This service is currently restoring its ',
		   'persistent database.', br([]),
		   'Loaded ~D triples'-[Triples]
		 | Extra
		 ]),
	throw(http_reply(unavailable(HTML))).

%%	attach_account_info
%
%	Set   the   registered   user-database     from    the   setting
%	cliopatria:user_data.

attach_account_info :-
	setting(cliopatria:user_data, File),
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

%%	cp_welcome
%
%	Print welcome banner.

cp_welcome :-
	setting(http:port, Port),
	print_message(informational, cliopatria(welcome(Port))).


		 /*******************************
		 *	       POOLS		*
		 *******************************/

:- multifile
	http:create_pool/1.

%%	http:create_pool(+Pool) is semidet.
%
%	Create a thread-pool on-demand.

http:create_pool(sparql_query) :-
	debug(http(pool), 'Demand-creating pool ~q', [sparql_query]),
	setting(sparql:max_clients, Count),
	setting(sparql:stack_size, MB),
	Global is MB * 1024,
	Trail is MB * 1024,
	thread_pool_create(sparql_query,
			   Count,
			   [ global(Global),
			     trail(Trail)
			   ]).
http:create_pool(cliopatria) :-
	thread_pool_create(cliopatria,
			   50,
			   [
			   ]).


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(cliopatria(server_started(Port))) -->
	{ gethostname(Host),
	  http_location_by_id(root, Root)
	},
	[ 'Started ClioPatria server at port ~w'-[Port], nl,
	  'You may access the server at http://~w:~w~w'-[Host, Port, Root]
	].
prolog:message(cliopatria(welcome(DefaultPort))) -->
	[ nl,
	  'Use one of the calls below to start the ClioPatria server:', nl, nl,
	  '  ?- cp_server.               % start at port ~w'-[DefaultPort], nl,
	  '  ?- cp_server([port(Port)]). % start at Port'
	].


		 /*******************************
		 *	        HOOKS		*
		 *******************************/

:- multifile
	user:message_hook/3.

user:message_hook(rdf(restore(_, done(_DB, _T, _Count, Nth, Total))),
		  _Kind, _Lines) :-
	retractall(loading_done(_,_)),
	assert(loading_done(Nth, Total)),
	fail.
