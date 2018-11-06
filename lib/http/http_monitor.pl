/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2018, University of Amsterdam, VU University Amsterdam
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

:- module(http_monitor,
          [ http_monitor/1              % +Options
          ]).
:- use_module(library(http/http_open)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(unix)).
:- use_module(library(time)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library(readutil)).

/** <module> Monitor an HTTP servers health status

This module monitors an HTTP server and kills it if it proves to be
in a bad state for some time.

*/

:- dynamic
    options/1,                      % Options
    started_at/1,                   % Stamp
    failures/1.                     % Current failure count

%!  http_monitor(+Options) is det.
%
%   Monitor an HTTP service.  Options:
%
%       * fork(+Bool)
%       If =true=, fork and monitor the parent process.  Unix only.
%
%       * pid(+PID)
%       The PID of the monitored process.  This process is killed if
%       it fails to respond
%
%       * interval(+Seconds)
%       Polling interval.  Default is to poll every 20 seconds.
%
%       * max_failures(+Count)
%       Terminate the monitored service after Count failed connects.
%       Default is 3 times.
%
%       * max_start_time(+Time)
%       Max time we consider 503 status acceptable.
%
%       * max_wait(+Wait)
%       Max time to wait for a reply.  Default is 10 seconds.
%
%       * url(URL)
%       URL to poll.  Default is =|http://localhost:<port>/|=

http_monitor(Options) :-
    select_option(fork(true), Options, Options1),
    !,
    current_prolog_flag(pid, Monitored),
    fork(Pid),
    (   Pid == client
    ->  http_monitor([pid(Monitored)|Options1])
    ;   print_message(informational, http_monitor(forked(Pid)))
    ).
http_monitor(Options) :-
    setup_monitor(Options),
    get_time(Now),
    assert(started_at(Now)),
    option(interval(Time), Options, 20),
    repeat,
        catch(check(Options), E,
              (   print_message(error, E),
                  halt(1))),
        sleep(Time),
        fail.

setup_monitor(Options) :-
    assert(options(Options)),
    on_signal(int, _, signal_handler),
    on_signal(hup, _, signal_handler).

signal_handler(int) :-
    halt.
signal_handler(hup) :-
    options(Options),
    check(Options).

check(Options) :-
    monitor_url(Options, URL),
    option(max_wait(Limit), Options, 10),
    get_time(Start),
    (   catch(call_with_time_limit(Limit,
                                   guarded_check(URL, Options)),
              E, true)
    ->  get_time(Now),
        Time is Now - Start,
        (   var(E)
        ->  debug(http_monitor, '200 OK in ~3f sec', [Time]),
            retractall(failures(_))
        ;   act(E, URL, Options)
        )
    ;   act(failed, URL, Options)
    ).

monitor_url(Options, URL) :-
    option(url(URL), Options),
    !.
monitor_url(_, URL) :-
    http_current_server(_Pred, Port),
    format(atom(URL), 'http://localhost:~w/', [Port]).

guarded_check(URL, Options) :-
    http_open(URL, In, Options),
    call_cleanup(read_stream_to_codes(In, _Codes),
                 close(In)).


act(error(_, context(status(Code, Comment))), URL, Options) :-
    !,
    print_message(warning, http_monitor(error(Code-Comment, URL))),
    step_error(Code, Options).
act(What, URL, Options) :-
    print_message(warning, http_monitor(error(What, URL))),
    step_error(0, Options).

step_error(503, Options) :-            % unavailable: startup
    !,
    started_at(Start),
    get_time(Now),
    Starting is Now - Start,
    option(max_start_time(MaxStart), Options, 300),
    (   Starting < MaxStart
    ->  true
    ;   print_message(error, http_monitor(start_failed(MaxStart))),
        kill_server(Options)
    ).
step_error(_, Options) :-
    (   retract(failures(C0))
    ->  true
    ;   C0 = 0
    ),
    C1 is C0+1,
    assert(failures(C1)),
    option(max_failures(Max), Options, 3),
    C1 > Max,
    print_message(error, http_monitor(no_response(Max))),
    kill_server(Options).

%!  kill_server(+Options) is semidet.
%
%   Kill the server.

kill_server(Options) :-
    option(pid(Pid), Options),
    !,
    between(1, 10, Try),
       signal_for_try(Try, Sig),
       print_message(informational, http_monitor(kill(Pid, Sig))),
       catch(kill(Pid, Sig), E, Done=true),
       (   Done == true
       ->  (   E = error(existence_error(process, Pid), _)
           ->  !
           ;   throw(E)
           )
       ;   sleep(10),
           fail
       ).
kill_server(_Options) :-
    existence_error(option, pid).

signal_for_try(Try, Sig) :-
    (   Try < 3
    ->  Sig = int
    ;   Try < 5
    ->  Sig = term
    ;   Sig = kill
    ).


                 /*******************************
                 *           MESSAGES           *
                 *******************************/

:- multifile
    prolog:message//1.

prolog:message(http_monitor(Message)) -->
    message(Message).

message(forked(PID)) -->
    [ 'Forked monitor process; PID=~w'-[PID] ].
message(kill(Pid, Signal)) -->
    [ 'Killing ~w using signal ~w'-[Pid, Signal] ].
message(error(Code-Comment, URL)) -->
    [ '~w: ~w (~w)'-[URL, Code, Comment] ].
message(error(Error, URL)) -->
    [ '~w: ~w'-[URL, Error] ].
message(start_failed(MaxTime)) -->
    [ 'Server claims 503 status for more than ~w seconds: killing'-[MaxTime] ].
message(no_response(Max)) -->
    [ 'Server failed to respond more than ~w times in a row: killing'-[Max] ].
