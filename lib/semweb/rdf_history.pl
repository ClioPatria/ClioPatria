/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

:- module(rdf_history,
	  [ rdfh_transaction/1,		% :Goal
	    rdfh_assert/3,		% +S,+P,+O
	    rdfh_retractall/3,		% +S,+P,+O
	    rdfh_update/3,		% +S[->NS],+P[->NP],+O[->[NO]
	    rdfh_db_transaction/3,	% ?DB, +Condition, ?Transaction
	    rdfh_triple_transaction/2,	% +Triple, -Transaction
	    rdfh_transaction_member/2	% ?Action, +Transaction
	  ]).
:- use_module(library('http/http_session')).
:- use_module(library(lists)).
:- use_module(library(record)).
:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library('semweb/rdf_persistency')).
:- use_module(library('semweb/rdf_db')).


/** <module> RDF Persistent store change history

This  module  deals  with  accessing  the   journal  files  of  the  RDF
persistency layer to get insight in the   provenance  and history of the
RDF database. It is designed for   Wiki-like collaborative editing of an
RDF graph. We make the following assumptions:

 * Users are identified using a URI, typically an OpenID (http://openid.net/)
 * Triples created by a user are added to a named graph identified by the
   URI of the user.
 * Changes are grouped using rdf_transaction(Goal, log(Message, User))
 * The number that is associated with the named graph of a triple (normally
   expressing the line number in the source) is used to store the time-stamp.
   Although this information is redundant (the time stamp is the same as
   for the transaction), it allows for binary search through the history
   file for the enclosing transaction.

@tbd	Cleanup thoughts on delete and update.

@author	Jan Wielemaker
*/

		 /*******************************
		 *	   DECLARATIONS		*
		 *******************************/

:- module_transparent
	rdfh_transaction/1.

:- rdf_meta
	rdfh_assert(r,r,o),
	rdfh_retractall(r,r,o),
	rdfh_update(t,t,t).

:- multifile
	rdfh_hook/1.

:- record
	rdf_transaction(id:integer,
			nesting:integer,
			time:number,
			message,
			actions:list,
			other_graphs:list).


		 /*******************************
		 *	   MODIFICATIONS	*
		 *******************************/

%%	rdfh_transaction(:Goal) is semidet.
%
%	Run Goal using rdf_transaction/2, using information from the HTTP
%	layer to provide OpenID and session-id.

rdfh_transaction(Goal) :-
	rdfh_user(User),
	transaction_context(Context),
	rdf_transaction(Goal, log(rdfh([user(User)|Context]), User)).


%%	rdfh_assert(+S, +P, +O) is det.
%
%	Assert a triple, adding current  user   and  time  to the triple
%	context.

rdfh_assert(S,P,O) :-
	(   rdf_active_transaction(log(rdfh(_), User))
	->  rdfh_time(Time),
	    rdf_assert(S,P,O,User:Time)
	;   throw(error(permission_error(assert, triple, rdf(S,P,O)),
			context(_, 'No rdfh_transaction/1')))
	).


%%	rdfh_retractall(+S, +P, +O) is det.
%
%	Retract triples that  match  {S,P,O}.   Note  that  all matching
%	triples are added to the journal, so   we can undo the action as
%	well as report on  retracted  triples,   even  if  multiple  are
%	retracted at the same time.
%	
%	One of the problems we are faced   with is that a retract action
%	goes into the journal of  the   user  whose triple is retracted,
%	which may or may not be the one who performed the action.

rdfh_retractall(S,P,O) :-
	(   rdf_active_transaction(log(rdfh(_), _User))
	->  rdf_retractall(S,P,O)
	;   throw(error(permission_error(retract, triple, rdf(S,P,O)),
			context(_, 'No rdfh_transaction/1')))
	).


%%	rdfh_update(+S, +P, +O) is det.
%
%	More tricky stuff, replacing a triple by another. Typically this
%	will be changing the predicate or object. Provenance info should
%	move the new triple to the user making the change, surely if the
%	object is changed. If the  predicate   is  changed  to a related
%	predicate, this actually becomes less obvious.
%	
%	Current simple-minded approach is  to  turn   an  update  into a
%	retract and assert. The S,P,O specifications are either a ground
%	value or of the form _Old_ =|->|= _New_. Here is an example:
%	
%	==
%	rdfh_update(Work, Style, wn:oldstyle -> wn:newstyle)
%	==

rdfh_update(S,P,O) :-
	(   rdf_active_transaction(log(rdfh(_), User))
	->  update(S,P,O, rdf(RS, RP, RO), rdf(AS, AP, AO)),
	    must_be(ground, RS),
	    must_be(ground, RP),
	    must_be(ground, RO),
	    rdfh_time(Time),
	    rdf_retractall(RS, RP, RO),
	    rdf_assert(AS, AP, AO, User:Time)
	;   throw(error(permission_error(retract, triple, rdf(S,P,O)),
			context(_, 'No rdfh_transaction/1')))
	).

update(Ss, Ps, Os, rdf(S0, P0, O0), rdf(S,P,O)) :-
	update(Ss, S0, S),
	update(Ps, P0, P),
	update(Os, O0, O).

update(From->To, From, To) :- !.
update(Value, Value, Value).


%%	transaction_context(-Term) is det.
%
%	Context to pass with an RDF transaction.   Note that we pass the
%	user. We don't need this for simple additions, but we do need it
%	to track deletions.

transaction_context(Context) :-
	(   rdfh_session(Session)
	->  Context = [session(Session)]
	;   Context = []
	).

%%	rdfh_session(-Session) is semidet.
%
%	Session is a (ground) identifier for the current session.

rdfh_session(Session) :-
	rdfh_hook(session(Session)), !.
rdfh_session(Session) :-
	catch(http_session_id(Session), _, fail).


%%	rdfh_user(-URI) is det.
%
%	Get user-id of current session.
%	
%	@tbd	Make hookable, so we can use the SeRQL user/openid hooks

rdfh_user(User) :-
	rdfh_hook(user(User)), !.
rdfh_user(OpenId) :-
	http_session_data(openid(OpenId)).

%%	rdfh_time(-Time:integer) is det.
%
%	Get time stamp as integer.  Second resolution is enough, and
%	avoids rounding problems associated with floats.

rdfh_time(Seconds) :-
	get_time(Now),
	Seconds is round(Now).


		 /*******************************
		 *	 EXAMINE HISTORY	*
		 *******************************/

%%	rdfh_triple_transaction(+Triple:rdf(S,P,O), -Transaction) is nondet.
%
%	True if the (partial) Triple is modified in Transaction.

rdfh_triple_transaction(rdf(S,P,O), Transaction) :-
	rdf(S,P,O,DB:Time),
	After is Time - 1,
	rdfh_db_transaction(DB, after(After), Transaction),
	rdfh_transaction_member(assert(S,P,O,Time), Transaction).

%%	rdfh_db_transaction(?DB, +Condition, ?Transaction) is nondet.
%
%	True if Transaction satisfying  Condition   was  executed on DB.
%	Condition is one of:
%	
%	  * true
%	  Always true, returns all transactions.
%	  * id(Id)
%	  Specifies the identifier of the transaction.  Only makes sense
%	  if DB is specified as transaction identifiers are local to each
%	  DB.
%	  * after(Time)
%	  True if transaction is executed at or after Time.
%	  
%	  @tbd	More conditions (e.g. before(Time)).

rdfh_db_transaction(DB, true, Transaction) :- !,
	rdf_journal_file(DB, Journal),
	journal_transaction(Journal, Transaction).
rdfh_db_transaction(DB, id(Id), Transaction) :- !,
	must_be(atom, DB),
	rdf_journal_file(DB, Journal),
	open_journal(Journal, Fd),
	call_cleanup((seek_journal(Fd, id(Id)),
		      read_transaction(Fd, Transaction)),
		     close(Fd)).
rdfh_db_transaction(DB, Condition, Transaction) :- !,
	valid_condition(Condition),
	rdf_journal_file(DB, Journal),
	open_journal(Journal, Fd),
	seek_journal(Fd, Condition),
	stream_transaction(Fd, Transaction).

valid_condition(Var) :-
	var(Var), !,
	instantiation_error(Var).
valid_condition(after(Time)) :- !,
	must_be(number, Time).
valid_condition(Cond) :-
	type_error(condition, Cond).

%%	open_journal(+File, -Stream) is det.
%
%	Open a journal file.  Journal files are always UTF-8 encoded.

open_journal(JournalFile, Fd) :-
	open(JournalFile, read, Fd, [encoding(utf8)]).

%%	journal_transaction(+JournalFile, ?Transaction) is nondet.
%
%	True if Transaction is a transaction in JournalFile,

journal_transaction(JournalFile, Transaction) :-
	open_journal(JournalFile, Fd),
	stream_transaction(Fd, Transaction).

stream_transaction(JFD, Transaction) :-
	call_cleanup(read_transaction(JFD, Transaction), close(JFD)).

read_transaction(In, Transaction) :-
	repeat,
	   read(In, T0),
	(   T0 == end_of_file
	->  !, fail
	;   transaction(T0, In, T),	% transaction/3 is not steadfast
	    T = Transaction
	).

transaction(begin(Id, Nest, Time, Msg), In,
	    rdf_transaction(Id, Nest, Time, Msg, Actions, Others)) :- !,
	read(In, T2),
	read_transaction_actions(T2, Id, In, Actions, Others).
transaction(start(_), _, _) :- !, fail.	% Open journal
transaction(end(_), _, _) :- !, fail.   % Close journal
transaction(Action, _, Action).		% Action outside transaction?

read_transaction_actions(end(Id, _, Others), Id, _, [], Others) :- !.
read_transaction_actions(end_of_file, _, _, [], []) :- !. % TBD: Incomplete transaction (error)
read_transaction_actions(Action, Id, In, Actions, Others) :-
	ignore_in_transaction(Action), !,
	read(In, T2),
	read_transaction_actions(T2, Id, In, Actions, Others).
read_transaction_actions(Action, Id, In, [Action|Actions], Others) :-
	read(In, T2),
	read_transaction_actions(T2, Id, In, Actions, Others).

ignore_in_transaction(start(_)).
ignore_in_transaction(end(_)).
ignore_in_transaction(begin(_,_,_,_)).
ignore_in_transaction(end(_,_,_)).


%%	seek_journal(+Fd:stream, +Spec) is semidet.
%
%	See an open journal descriptor to the start of a transaction
%	specified by Spec.  Spec is one of:
%	
%	  * after(Time)
%	  First transaction at or after Time.  Fails if there are no
%	  transactions after time.
%	  * id(Id)
%	  Start of transaction labeled with given Id.  Fails if there
%	  is no transaction labeled Id.
%	
%	The implementation relies on the incrementing identifier numbers
%	and time-stamps.

seek_journal(Fd, Spec) :-
	stream_property(Fd, file_name(File)),
	size_file(File, Size),
	Here is Size//2,
	Last = last(-),
	(   is_after_spec(Spec)
	->  (   bsearch_journal(Fd, 0, Here, Size, Spec, Last)
	    ->	true
	    ;	arg(1, Last, StartOfTerm),
		StartOfTerm \== (-),
		seek(Fd, StartOfTerm, bof, _)
	    )
	;   bsearch_journal(Fd, 0, Here, Size, Spec, Last)
	).

is_after_spec(after(_Time)).

%%	bsearch_journal(+Fd, +Start, +Here, +End, +Spec, !Last) is semidet.
%
%	Perform a binary search in the journal opened as Fd.

bsearch_journal(Fd, Start, Here, End, Spec, Last) :-
	start_of_transaction(Fd, Here, StartOfTerm, Begin), !,
	compare_transaction(Spec, Begin, Diff),
	(   Diff == (=)
	->  seek(Fd, StartOfTerm, bof, _)
	;   Diff == (<)
	->  NewHere is Start+(Here-Start)//2,
	    NewHere < Here,
	    nb_setarg(1, Last, StartOfTerm),
	    bsearch_journal(Fd, Start, NewHere, Here, Spec, Last)
	;   NewHere is StartOfTerm+(End-StartOfTerm)//2,
	    NewHere > StartOfTerm,
	    bsearch_journal(Fd, StartOfTerm, NewHere, End, Spec, Last)
	).
bsearch_journal(Fd, Start, Here, _End, Spec, Last) :-
	NewHere is Start+(Here-Start)//2,
	NewHere	< Here,
	bsearch_journal(Fd, Start, NewHere, Here, Spec, Last).

compare_transaction(id(Id), begin(Id2,_,_,_), Diff) :- !,
	compare(Diff, Id, Id2).
compare_transaction(after(Time), begin(_,_,T,_), Diff) :- !,
	compare(Diff, Time, T).

%%	start_of_transaction(+Fd, +From, -Start, -Term) is semidet.
%
%	Term is the start  term  of   the  first  transaction after byte
%	position From. Fails if no transaction can be found after From.

start_of_transaction(Fd, From, Start, Term) :-
	seek(Fd, From, bof, _),
	skip(Fd, 10),
	repeat,
	    seek(Fd, 0, current, Start),
	    read(Fd, Term),
	    (	transaction_start(Term)
	    ->	!
	    ;	Term == end_of_file
	    ->	!, fail
	    ;	fail
	    ).

transaction_start(begin(_Id,_Nest,_Time,_Message)).

%%	rdfh_transaction_member(Action, Transaction) is nondet.
%
%	True if Action is an action in Transaction.

rdfh_transaction_member(Action, Transaction) :-
	rdf_transaction_actions(Transaction, Actions),
	member(Action, Actions).
