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

:- module(serql_log,
	  [ serql_log_stream/1,		% -Stream
	    serql_log/2,		% +Format, +Args
	    log_started/3,		% +Request, -Id, +Stream
	    log_completed/4		% +Reason, +Id, +CPU0, +Stream
	  ]).
:- use_module(library(settings)).
:- use_module(library(broadcast)).
:- use_module(library(debug)).
:- use_module(user_db).

:- setting(http:log_requests_in, atom, 'requests.log',
	   'File in which to log HTTP requests').

% If the log settings change,  simply  close   the  log  and  it will be
% reopened with the new settings.

:- listen(settings(changed(http:log_requests_in, _, New)),
	  with_mutex(serql_log, close_log(changed(New)))).


		 /*******************************
		 *	   LOG ACTIVITY		*
		 *******************************/

:- dynamic
	log_stream/1.

%%	serql_log_stream(-Stream) is semidet.
%	
%	Returns handle to open logfile. Fails if no logfile is open and
%	none is defined.

serql_log_stream(Stream) :-
	log_stream(Stream), !,
	Stream \== [].
serql_log_stream(Stream) :-
	setting(http:log_requests_in, File),
	File \== '', !,
	with_mutex(serql_log,
		   (   open(File, append, Stream,
			    [ close_on_abort(false),
			      encoding(utf8)
			    ]),
		       get_time(Time),
		       format(Stream,
			      'server(started, ~0f).~n',
			      [ Time ]),
		       assert(log_stream(Stream)),
		       at_halt(close_log(stopped))
		   )).
serql_log_stream(_) :-
	assert(log_stream([])).

close_log(Reason) :-
	retract(log_stream(Stream)), !,
	(   Stream == []
	->  true
	;   get_time(Time),
	    format(Stream, 'server(~q, ~0f).~n', [ Reason, Time ]),
	    close(Stream)
	).
close_log(_).

%%	serql_log(+Format, +Args) is det.
%
%	Write message from Format and Args to log-stream.  See format/2
%	for details.

serql_log(Format, Args) :-
	(   serql_log_stream(Stream)
	->  format(Stream, Format, Args)
	;   true
	).


%%	log_started(+Request, -Id, +Stream) is det.
%
%	Write log message that Request was started to Stream.
%	
%	@param	Filled with sequence identifier for the request

log_started(Request, Id, Stream) :-
	get_time(Now),
	flag(serql_request_id, Id, Id+1),
	log_request(Request, LogRequest),
	catch(logged_on(User), _, User = anonymous),
	format_time(string(HDate), '%+', Now),
	format(Stream,
	       '/*~s*/ request(~q, ~0f, ~q, ~q).~n',
	       [HDate, Id, Now, User, LogRequest]),
	flush_output(Stream),
	memberchk(path(Path), LogRequest),
	debug(http(request), '[~D] ~q ...', [Id, Path]).

%%	log_request(+Request, -Log)
%	
%	Remove passwords from the request to avoid sending them to the
%	logfiles.

log_request([], []).
log_request([search(Search0)|T0], [search(Search)|T]) :- !,
	mask_passwords(Search0, Search),
	log_request(T0, T).
log_request([H|T0], T) :-
	nolog(H), !,
	log_request(T0, T).
log_request([H|T0], [H|T]) :-
	log_request(T0, T).

mask_passwords([], []).
mask_passwords([Name=_|T0], [Name=xxx|T]) :-
	pwd_entry(Name), !,
	mask_passwords(T0, T).
mask_passwords([H|T0], [H|T]) :-
	mask_passwords(T0, T).

pwd_entry(password).
pwd_entry(pwd0).
pwd_entry(pwd1).
pwd_entry(pwd2).

nolog(input(_)).


%%	log_completed(+Status, +Id, +CPU0, +Stream) is det.
%
%	Write log message to Stream from a call_cleanup/3 call.
%	
%	@param Status	2nd argument of call_cleanup/3
%	@param Id	Term identifying the completed request
%	@param CPU0	CPU time at time of entrance
%	@param Stream	Stream to write to (normally from serql_log_stream/1).

log_completed(Status, Id, CPU0, Stream) :-
	thread_self(Me),
	thread_statistics(Me, cputime, CPU1),
	CPU is CPU1 - CPU0,
	(   debugging(http(request))
	->  debug_request(Status, Id, CPU)
	;   true
	),
	is_stream(Stream), !,
	log(Status, Id, CPU, Stream),
	flush_output(Stream).
log_completed(Status, Id, CPU0, _) :-
	serql_log_stream(Stream), !,	% Logfile has changed!
	log_completed(Status, Id, CPU0, Stream).
log_completed(_,_,_,_).


log(exit, Id, CPU, Stream) :-
	format(Stream, 'completed(~q, ~2f, true).~n',
	       [ Id, CPU ]).
log(!, Id, CPU0, Stream) :- !,
	log_completed(exit, Id, CPU0, Stream).
log(fail, Id, CPU, Stream) :-
	format(Stream, 'completed(~q, ~2f, false).~n',
	       [ Id, CPU ]).
log(exception(Except), Id, CPU, Stream) :-
	(   map_exception(Except, Term)
	->  true
	;   message_to_string(Except, String),
	    Term = error(String)
	),
	format(Stream, 'completed(~q, ~2f, ~q).~n',
	       [ Id, CPU, Term ]).

map_exception(http_reply(Reply), Reply).
map_exception(error(existence_error(http_location, Location), _Stack),
	      error(404, Location)).

%%	debug_request(+Status, +Id, +CPU0)
%
%	Emit debugging info after a request completed with Status.

debug_request(exception(Except), Id, _) :-
	map_exception(Except, Reply), !,
	debug(http(request), '[~D] ~w', [Id, Reply]).
debug_request(exception(Except), Id, _) :- !,
	message_to_string(Except, Message),
	debug(http(request), '[~D] ERROR: ~w', [Id, Message]).
debug_request(exit, Id, CPU) :- !,
	debug(http(request), '[~D] exit (~3f seconds)', [Id, CPU]).
debug_request(Status, Id, _) :-
	debug(http(request), '[~D] ~w', [Id, Status]).
