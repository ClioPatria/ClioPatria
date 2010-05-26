/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam, VU University Amsterdam

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

:- module(http_monitor,
	  [ http_monitor/1		% +Options
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
	options/1,			% Options
	started_at/1,			% Stamp
	failures/1.			% Current failure count

%%	http_monitor(+Options) is det.
%
%	Monitor an HTTP service.  Options:
%
%	    * fork(+Bool)
%	    If =true=, fork and monitor the parent process.  Unix only.
%
%	    * pid(+PID)
%	    The PID of the monitored process.  This process is killed if
%	    it fails to respond
%
%	    * interval(+Seconds)
%	    Polling interval.  Default is to poll every 20 seconds.
%
%	    * max_failures(+Count)
%	    Terminate the monitored service after Count failed connects.
%	    Default is 3 times.
%
%	    * max_start_time(+Time)
%	    Max time we consider 503 status acceptable.
%
%	    * max_wait(+Wait)
%	    Max time to wait for a reply.  Default is 10 seconds.
%
%	    * url(URL)
%	    URL to poll.  Default is =|http://localhost:<port>/|=

http_monitor(Options) :-
	select_option(fork(true), Options, Options1), !,
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
	    ->	debug(http_monitor, '200 OK in ~3f sec', [Time]),
	        retractall(failures(_))
	    ;	act(E, URL, Options)
	    )
	;   act(failed, URL, Options)
	).

monitor_url(Options, URL) :-
	option(url(URL), Options), !.
monitor_url(_, URL) :-
	http_current_server(_Pred, Port),
	format(atom(URL), 'http://localhost:~w/', [Port]).

guarded_check(URL, Options) :-
	http_open(URL, In, Options),
	call_cleanup(read_stream_to_codes(In, _Codes),
		     close(In)).


act(error(_, context(status(Code, Comment))), URL, Options) :- !,
	print_message(warning, http_monitor(error(Code-Comment, URL))),
	step_error(Code, Options).
act(What, URL, Options) :-
	print_message(warning, http_monitor(error(What, URL))),
	step_error(0, Options).

step_error(503, Options) :- !,		% unavailable: startup
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

%%	kill_server(+Options) is semidet.
%
%	Kill the server.

kill_server(Options) :-
	option(pid(Pid), Options), !,
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
		 *	     MESSAGES		*
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
