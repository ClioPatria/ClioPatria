/*  Part of ClioPatria

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://cliopatria.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
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

:- module(cp_server,
          [ cp_server/0,
            cp_server/1,                % +Options
            cp_welcome/0,
            cp_after_load/1             % :Goal
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

  :- use_module(cliopatria(cliopatria)).
  ==

@see http://cliopatria.swi-prolog.org
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

user:file_search_path(library, cliopatria(lib)).

:- use_module(library(version)).
:- check_prolog_version(or(70600, 70514)).              % Demand >= 7.6.0, 7.5.14
:- register_git_module('ClioPatria',
                       [ home_url('http://cliopatria.swi-prolog.org/')
                       ]).

:- use_module([ parms,
                skin(cliopatria),                       % HTML Page layout
                library(option),
                library(bundle),
                library(debug),
                library(lists),
                library(settings),
                library(error),
                library(broadcast),
                library(thread_pool),
                library(apply),

                library(semweb/rdf_db),
                library(semweb/rdf_persistency),
                library(semweb/rdf_litindex),
                library(semweb/rdf_ntriples),

                library(http/http_session),
                library(http/http_server_files),
                library(http/http_dispatch),
                library(http/thread_httpd),

                user(user_db),
                user(openid),
                user(preferences),

                api(sesame),
                api(journal),                   % export journal information
                api(sparql),
                api(export),
                api(void),

                applications(admin),
                applications(user),
                applications(browse),
                applications(yasgui),

                library(conf_d),
                user:library(cpack/cpack)
              ]).

:- if(exists_source(library(http/http_dyn_workers))).
:- use_module(library(http/http_dyn_workers)).
:- endif.

:- http_handler(web(.), serve_files_in_directory(web), [prefix]).

:- dynamic
    after_load_goal/1.

%!  cp_server is det.
%!  cp_server(:Options) is det.
%
%   Start the HTTP server.  This predicate preforms the following
%   steps:
%
%       1. Load application settings from =|settings.db|=
%       2. Load user-data from =|users.db|=
%       3. Start the HTTP server
%       4. Load the RDF persistent database from =|RDF-store|=
%       5. Execute `after load' options registered using
%          cp_after_load/1.
%
%   Defined options are:
%
%       * port(Port)
%       Attach to Port instead of the port specified in the
%       configuration file settings.db.
%       * workers(+Count)
%       Number of worker threads to use.  Default is the setting
%       =|http:workers|=
%       * prefix(+Prefix)
%       Rebase the server.  See also the setting =|http:prefix|=.
%       * store(+Store)
%       Directory to use as persistent store. See also the
%       setting =|cliopatria:persistent_store|=.
%       * settings(+Settings)
%       Settings file.  Default is =settings.db=.

:- meta_predicate
    cp_server(:).

cp_server :-
    current_prolog_flag(argv, [cpack|Argv]),
    !,
    load_conf_d([ 'config-enabled' ], []),
    cpack_control(Argv).
:- if(current_predicate(http_unix_daemon:http_daemon/0)).
cp_server :-
    http_unix_daemon:http_daemon.
:- else.
cp_server :-
    process_argv(Options, PrologFiles, RDFInputs),
    load_application(Options),
    user:maplist(ensure_loaded, PrologFiles),
    catch(cp_server([rdf_load(RDFInputs)|Options]), E, true),
    (   var(E)
    ->  set_prolog_flag(toplevel_goal, prolog) % become interactive
    ;   print_message(error, E),
        (   E = error(socket_error('Address already in use'), _)
        ->  print_message(error, cliopatria(use_port_option))
        ;   true
        )
    ).
:- endif.

cp_server(_Options) :-
    setting(http:port, DefPort),
    http_server_property(DefPort, goal(cp_server:http_dispatch)),
    !,
    print_message(informational,
                  cliopatria(server_already_running(DefPort))).
cp_server(Options) :-
    meta_options(is_meta, Options, QOptions),
    load_application(QOptions),
    option(settings(SettingsFile), QOptions, 'settings.db'),
    load_settings(SettingsFile),
    set_prefix(QOptions),
    attach_account_info,
    set_session_options,
    create_log_directory,
    setting(http:port, DefPort),
    setting(http:workers, DefWorkers),
    setting(http:worker_options, Settings),
    https_options(HTTPSOptions),
    merge_options(QOptions, Settings, HTTPOptions0),
    merge_options(HTTPOptions0, HTTPSOptions, HTTPOptions),
    option(port(Port), QOptions, DefPort),
    update_public_port(Port, DefPort),
    option(workers(Workers), QOptions, DefWorkers),
    http_server(http_dispatch,
                [ port(Port),
                  workers(Workers)
                | HTTPOptions
                ]),
    option(after_load(AfterLoad), QOptions, true),
    option(rdf_load(RDFInputs), QOptions, []),
    print_message(informational, cliopatria(server_started(Port))),
    setup_call_cleanup(
        http_handler(root(.), busy_loading,
                     [ priority(1000),
                       hide_children(true),
                       id(busy_loading),
                       prefix
                     ]),
        rdf_attach_store(QOptions, after_load(AfterLoad, RDFInputs)),
        http_delete_handler(id(busy_loading))).

is_meta(after_load).

:- public after_load/2.

:- meta_predicate
    after_load(0, +).

after_load(AfterLoad, RDFInputs) :-
    forall(member(Input, RDFInputs),
           call_warn(rdf_load(Input))),
    call(AfterLoad).

set_prefix(Options) :-
    option(prefix(Prefix), Options),
    \+ setting(http:prefix, Prefix),
    !,
    set_setting_default(http:prefix, Prefix).
set_prefix(_).

%!  update_public_port(+Port, +DefPort)
%
%   Update http:public_port if port is   changed  using --port=Port.
%   Without this hack it is no longer  to login after using the port
%   option.

update_public_port(Port, Port) :- !.
update_public_port(Port, DefPort) :-
    setting(http:public_port, DefPort),
    !,
    set_setting_default(http:public_port, Port),
    assertion(setting(http:public_port, Port)).
update_public_port(_, _).


%!  load_application(+Options)
%
%   Load cpack and local configuration.

:- dynamic
    application_loaded/0.
:- volatile
    application_loaded/0.

load_application(_Options) :-
    application_loaded,
    !.
load_application(_Options) :-
    load_conf_d([ cliopatria('config-enabled'),
                  'config-enabled'
                ], []),
    load_local,
    assertz(application_loaded).

load_local :-
    absolute_file_name(local, Local,
                       [ file_type(prolog),
                         access(read),
                         file_errors(fail)
                       ]),
    !,
    print_message(informational, conf_d(load(Local))),
    ensure_loaded(user:Local).
load_local.

%!  rdf_attach_store(+Options, :AfterLoad) is det.
%
%   Attach     the     RDF     store       using     the     setting
%   cliopatria:persistent_store and call the `after-load' goals.
%
%   @see cp_after_load/1 for registering after-load goals.

:- meta_predicate
    rdf_attach_store(+, 0),
    call_warn(0).

rdf_attach_store(Options, AfterLoad) :-
    (   option(store(Directory), Options)
    ->  true
    ;   setting(cliopatria:persistent_store, Directory)
    ),
    setup_indices,
    (   Directory \== ''
    ->  rdf_attach_db(Directory, Options)
    ;   true
    ),
    forall(after_load_goal(Goal),
           call_warn(Goal)),
    call_warn(AfterLoad).

call_warn(Goal) :-
    (   catch(Goal, E, true)
    ->  (   var(E)
        ->  true
        ;   print_message(warning, E)
        )
    ;   print_message(warning, goal_failed(Goal))
    ).


%!  setup_indices is det.
%
%   Initialize maintenance of the full-text   indices. These indices
%   are created on first call and  maintained dynamically as the RDF
%   store changes. By initializing them  before   there  is  any RDF
%   loaded, they will be built while  the data is (re-)loaded, which
%   avoids long delays on the first  query.   Note  that most of the
%   work is done in a separate thread.

setup_indices :-
    setting(cliopatria:pre_index_tokens, true),
    rdf_find_literals(not_a_token, _),
    fail.
setup_indices :-
    setting(cliopatria:pre_index_stems, true),
    rdf_find_literals(stem(not_a_stem), _),
    fail.
setup_indices.


%!  cp_after_load(:Goal) is det.
%
%   Register Goal to be executed after  reloading the RDF persistent
%   DB. Note that  already  registered   goals  are  not duplicated.
%   Running a goal after loading the   database  is commonly used to
%   ensure presence of relevant schemas or build additional indices.
%   Note that it is possible to   start  a thread for time-consuming
%   tasks (see thread_create/3).

:- meta_predicate
    cp_after_load(0).

cp_after_load(Goal) :-
    (   after_load_goal(Goal)
    ->  true
    ;   assert(after_load_goal(Goal))
    ).


%!  busy_loading(+Request)
%
%   This HTTP handler is  pushed  to   overrule  all  actions of the
%   server while the server is restoring   its  persistent state. It
%   replies with the 503  (unavailable)   response,  indicating  the
%   progress of restoring the repository.

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

%!  attach_account_info
%
%   Set   the   registered   user-database     from    the   setting
%   cliopatria:user_data.

attach_account_info :-
    setting(cliopatria:user_data, File),
    set_user_database(File).

%!  set_session_options
%
%   Initialise session timeout from =|http:max_idle_time|=.

set_session_options :-
    setting(http:max_idle_time, Idle),
    http_set_session_options([timeout(Idle)]).

%!  create_log_directory
%
%   Create the directory in which the log files reside.

create_log_directory :-
    current_setting(http:logfile),
    setting(http:logfile, File), File \== '',
    file_directory_name(File, DirName),
    DirName \== '.',
    !,
    catch(make_directory_path(DirName), E,
          print_message(warning, E)).
create_log_directory.


                 /*******************************
                 *       UPDATE SETTINGS        *
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
                 *             ARGV             *
                 *******************************/

%!  process_argv(-Options, -PrologFiles, -RDFInputs)
%
%   Processes the ClioPatria commandline options.

process_argv(Options, PrologFiles, RDFInputs) :-
    current_prolog_flag(argv, Argv),
    current_prolog_flag(os_argv, [Program|_]),
    (   Argv == ['--help']
    ->  usage(Program)
    ;   catch((   parse_options(Argv, Options, Rest),
                  maplist(load_argument, Rest, Load),
                  keysort(Load, Sorted),
                  group_pairs_by_key(Sorted, Keyed),
                  (   memberchk(prolog-PrologFiles, Keyed)
                  ->  true
                  ;   PrologFiles = []
                  ),
                  (   memberchk(rdf-RDFInputs, Keyed)
                  ->  true
                  ;   RDFInputs = []
                  )
              ),
              E,
              (   print_message(error, E),
                  fail
              ))
    ->  true
    ;   usage(Program)
    ).

load_argument(URL, rdf-URL) :-
    (   sub_atom('http://', 0, _, _, URL)
    ;   sub_atom('https://', 0, _, _, URL)
    ),
    !.
load_argument(File, Type-File) :-
    file_name_extension(_Base, Ext, File),
    load_argument(Ext, File, Type).

load_argument(Ext, _File, prolog) :-
    user:prolog_file_type(Ext, prolog),
    !.
load_argument(gz, File, rdf) :-
    file_name_extension(Plain, gz, File),
    file_name_extension(_, RDF, Plain),
    rdf_extension(RDF).
load_argument(RDF, _File, rdf) :-
    rdf_extension(RDF).

rdf_extension(rdf).
rdf_extension(owl).
rdf_extension(ttl).
rdf_extension(nt).
rdf_extension(ntriples).

cmd_option(-, help,       -,                'Print command usage').
cmd_option(p, port,       positive_integer, 'Port to connect to').
cmd_option(w, workers,    positive_integer, 'Number of workers to start').
cmd_option(-, after_load, term,             'Goal to run after loading').
cmd_option(-, prefix,     atom,             'Rebase the server to prefix/').
cmd_option(-, store,      atom,             'Directory for persistent store').
% dummy to stop list_trivial_fail from warning about long_option/2.
cmd_option(-, -, boolean, 'Dummy') :- fail.

usage(Program) :-
    format(user_error,
           'Run ClioPatria for interactive usage.~n~n', []),
    ansi_format([bold], 'Usage: ~w [options] arguments', [Program]), nl, nl,
    flush_output,
    forall(cmd_option(Short, Long, Type, Comment),
           describe_option(Short, Long, Type, Comment)),
    cpack_usage(Program),
    describe_argv,
    (   current_prolog_flag(hwnd, _)        % swipl-win.exe console
    ->  ansi_format([bold,hfg(red)],
                    '~nPress \'b\' for break, any other key to exit > ', []),
        get_single_char(Key),
        (   Key == 0'b
        ->  nl, nl, break
        ;   true
        ),
        halt
    ;   halt(1)
    ).

describe_option(-, Long, -, Comment) :-
    !,
    format(user_error, '    --~w~t~40|~w~n', [Long, Comment]).
describe_option(-, Long, _, Comment) :-
    !,
    format(user_error, '    --~w=~w~t~40|~w~n', [Long, Long, Comment]).
describe_option(Short, Long, -, Comment) :-
    !,
    format(user_error, '    -~w, --~w~t~40|~w~n',
           [Short, Long, Comment]).
describe_option(Short, Long, _, Comment) :-
    !,
    format(user_error, '    -~w ~w, --~w=~w~t~40|~w~n',
           [Short, Long, Long, Long, Comment]).

describe_argv :-
    current_prolog_flag(argv, Argv),
    (   Argv == ['--help']
    ->  true
    ;   ansi_format([fg(red)], 'Program argv: ~q~n', [Argv])
    ).

cpack_usage(Program) :-
    nl, ansi_format([bold], 'CPACK commands', []), nl, nl,
    flush_output,
    format(user_error, '   ~w cpack install pack ...~n', [Program]),
    format(user_error, '   ~w cpack upgrade pack ...~n', [Program]),
    format(user_error, '   ~w cpack configure pack ...~n', [Program]).

parse_options([], [], []).
parse_options([--|Rest], [], Rest) :- !.
parse_options([H|T], [Opt|OT], Rest) :-
    sub_atom(H, 0, _, _, --),
    !,
    (   sub_atom(H, B, _, A, =)
    ->  B2 is B - 2,
        sub_atom(H, 2, B2, _, Name),
        sub_atom(H, _, A,  0, Value),
        long_option(Name, Value, Opt)
    ;   sub_atom(H, 2, _, 0, Name),
        long_option(Name, Opt)
    ),
    parse_options(T, OT, Rest).
parse_options([H|T], Opts, Rest) :-
    atom_chars(H, [-|Opts]),
    !,
    short_options(Opts, T, Opts, Rest).
parse_options(Rest, [], Rest).

short_options([], Av, Opts, Rest) :-
    parse_options(Av, Opts, Rest).
short_options([H|T], Av, [Opt|OptT], Rest) :-
    cmd_option(H, Name, Type, _),
    (   Type == (-)
    ->  Opt =.. [Name,true],
        short_options(T, Av, OptT, Rest)
    ;   Av = [Av0|AvT],
        text_to_value(Type, Av0, Value),
        Opt =.. [Name,Value],
        short_options(T, AvT, OptT, Rest)
    ).

long_option(Name, Text, Opt) :-
    cmd_option(_, Name, Type, _),
    text_to_value(Type, Text, Value),
    Opt =.. [Name,Value].

long_option(Name, Opt) :-
    atom_concat('no-', OptName, Name),
    cmd_option(_, OptName, boolean, _),
    !,
    Opt =.. [Name,false].
long_option(Name, Opt) :-
    cmd_option(_, Name, boolean, _),
    Opt =.. [Name,true].

text_to_value(boolean, Text, Value) :-
    downcase_atom(Text, Lwr),
    boolean(Lwr, Value).
text_to_value(atom, Text, Text).
text_to_value(oneof(L), Text, Text) :-
    memberchk(Text, L).
text_to_value(integer, Text, Int) :-
    atom_number(Text, Int), integer(Int).
text_to_value(nonneg, Text, Int) :-
    atom_number(Text, Int), integer(Int), Int >= 0.
text_to_value(positive_integer, Text, Int) :-
    atom_number(Text, Int), integer(Int), Int > 0.
text_to_value(negative_integer, Text, Int) :-
    atom_number(Text, Int), integer(Int), Int < 0.
text_to_value(float, Text, Float) :-
    atom_number(Text, Number), Float = float(Number).
text_to_value(term, Text, Term) :-
    atom_to_term(Text, Term, _).

boolean(true,  true).
boolean(yes,   true).
boolean(on,    true).
boolean(false, false).
boolean(no,    false).
boolean(off,   false).


                 /*******************************
                 *             CPACK            *
                 *******************************/

%!  cpack_control(+Commands:list)
%
%   Execute a CPACK configuration instruction.  For example:
%
%       ./run.pl cpack install swish

cpack_control([install|Packs]) :-
    !,
    maplist(cpack_install, Packs).
cpack_control([configure|Packs]) :-
    !,
    maplist(cpack_configure, Packs).
cpack_control([upgrade|Packs]) :-
    !,
    (   Packs == []
    ->  cpack_upgrade
    ;   maplist(cpack_upgrade, Packs)
    ).
cpack_control(Command) :-
    domain_error(cpack_command, Command).


                 /*******************************
                 *            BANNER            *
                 *******************************/

%!  cp_welcome
%
%   Print welcome banner.

cp_welcome :-
    setting(http:port, Port),
    print_message(informational, cliopatria(welcome(Port))).


                 /*******************************
                 *             POOLS            *
                 *******************************/

:- multifile
    http:create_pool/1.

:- setting(cliopatria:max_clients, integer, 50,
           'Max number of concurrent requests in ClioPatria pool').
:- if(current_prolog_flag(address_bits, 32)).
:- setting(cliopatria:stack_size, integer, 128,
           'Stack limit in MB for ClioPatria pool').
:- else.
:- setting(cliopatria:stack_size, integer, 1024,
           'Stack limit in MB for ClioPatria pool').
:- endif.

%!  http:create_pool(+Pool) is semidet.
%
%   Create a thread-pool on-demand.

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
    setting(cliopatria:max_clients, Count),
    setting(cliopatria:stack_size, MB),
    Global is MB * 1024,
    Trail is MB * 1024,
    thread_pool_create(cliopatria,
                       Count,
                       [ global(Global),
                         trail(Trail)
                       ]).


                 /*******************************
                 *            HTTPS             *
                 *******************************/

%!  https_options(-Options) is det.
%
%   Fetch options for running an HTTPS   server.  HTTP is started if
%   there is a directory =https= with these files:
%
%     $ =|server-cert.pem|= :
%     Contains the server certificate.  This may be omitted, in
%     which case the =|server-key.pem|= is also passed using the
%     key_file(+File) option.
%     $ =|server-key.pem|= :
%     Contains the private key for the server.
%     % =|passwd|= :
%     Needs to hold the password if the private key is protected
%     with a password.

https_options(Options) :-
    https_file('server-key.pem', KeyFile),
    !,
    (   https_file('server-cert.pem', CertFile)
    ->  true
    ;   CertFile = KeyFile
    ),
    Options = [ ssl([ certificate_file(CertFile),
                      key_file(KeyFile)
                    | PasswdOption
                    ])
              ],
    (   https_file(passwd, PasswordFile)
    ->  read_file_to_string(PasswordFile, Content, []),
        split_string(Content, "", " \n\r", [Passwd]),
        PasswdOption = [password(Passwd)]
    ;   PasswdOption = []
    ).
https_options([]).

https_file(Base, File) :-
    absolute_file_name(config_https(Base), File,
                       [ access(read),
                         file_errors(fail)
                       ]).



                 /*******************************
                 *           MESSAGES           *
                 *******************************/

:- multifile
    prolog:message//1.

prolog:message(cliopatria(server_started(_Port))) -->
    [].
prolog:message(cliopatria(welcome(DefaultPort))) -->
    [ nl,
      'Use one of the calls below to start the ClioPatria server:', nl, nl,
      '  ?- cp_server.               % start at port ~w'-[DefaultPort], nl,
      '  ?- cp_server([port(Port)]). % start at Port'
    ].
prolog:message(cliopatria(use_port_option)) -->
    [ '   Could not start the HTTP server!', nl,
      '   Choose a different port using ./run.pl --port=<port> or', nl,
      '   use the network plugin to change the default port.'
    ].
prolog:message(cliopatria(server_already_running(Port))) -->
    { cp_host(Port, Host),
      cp_port(Port, PublicPort),
      http_location_by_id(root, Root)
    },
    [ 'CliopPatria server is already running at http://~w:~w~w'-
      [Host, PublicPort, Root]
    ].

cp_host(_, Host) :-
    setting(http:public_host, Host),
    Host \== '',
    !.
cp_host(Host:_, Host) :- !.
cp_host(_,Host) :-
    gethostname(Host).

cp_port(_ServerPort, PublicPort) :-
    setting(http:public_host, Host),
    Host \== '', Host \== localhost,
    setting(http:public_port, PublicPort),
    !.
cp_port(_Host:Port, Port) :- !.
cp_port(ServerPort, ServerPort).



                 /*******************************
                 *              HOOKS           *
                 *******************************/

:- multifile
    user:message_hook/3.

user:message_hook(rdf(restore(_, done(_DB, _T, _Count, Nth, Total))),
                  _Kind, _Lines) :-
    retractall(loading_done(_,_)),
    assert(loading_done(Nth, Total)),
    fail.

:- multifile
    http_unix_daemon:http_server_hook/1. % +Options

http_unix_daemon:http_server_hook(Options) :-
    cp_server(Options).
