/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2004, University of Amsterdam

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


:- module(db,
	  [ (db_term)/1,		% +Declarations

	    db_attach/2,		% :File, +Options

	    db_assert/1,		% :Term
	    db_retractall/1,		% :Term

	    db_sync/1,			% :What
	    db_sync_all/1,		% +What

	    op(1150, fx, (db_term))
	  ]).
:- use_module(library(debug)).
:- use_module(library(error)).

/** <module> Provide persistent dynamic predicates

This module provide simple persistent storage   for  one or more dynamic
predicates. A database is always associated with a module. A module that
wishes to maintain a database must declare the terms that can be placed
in the database using the predicate db_term(?Term).

db_term/1 is called by attach_db/1 to initialise all database predicates
as dynamic. In this  mode  it  is   called  as  db_term(-Term)  and must
enumerate all database terms. While loading a  database, it is called as
db_term(+Term) and must succeed  for  any   valid  database  term. If it
fails, a warning is printed  and  the   term  is  not  asserted into the
database.

Here is	a typical example:

==
db_term(user(_Name, _Group)).

	...,
	db_attach('users.db'),

	...,
	user(Name, Group),
	
	...,
	db_assert(user('Bob', 'administrator')),
==

@tbd	Deal with amount of `dirtyness'.  I.e. only _gc_ if more than
	_X_ percent is dirty.
@tbd	Provide db_retract/1?
@tbd	Type safety
@tbd	Thread safety
@tbd	Transaction management?

@author Jan Wielemaker
*/

:- meta_predicate
	db_attach(:, +),
	db_assert(:),
	db_retractall(:),
	db_sync(:).


		 /*******************************
		 *		DB		*
		 *******************************/

:- dynamic
	db_file/3,			% Module, File, Modified
	db_stream/2,			% Module, Stream
	db_dirty/1,			% Module
	db_option/2.			% Module, Name(Value)

:- multifile
	(db_term)/2.			% Module, Term


		 /*******************************
		 *	   DECLARATIONS		*
		 *******************************/

%%	db_term(+Spec)
%
%	Declare dynamic database terms. Declarations appear in a
%	directive and have the following format:
%	
%	==
%	:- db_term
%		<callable>,
%		<callable>,
%		...
%	==

db_term(Spec) :-
	throw(error(context_error(nodirective, db_term(Spec)), _)).

compile_db_term(Var) -->
	{ var(Var), !,
	  type_error(callable, Var)
	}.
compile_db_term((A,B)) --> !,
	compile_db_term(A),
	compile_db_term(B).
compile_db_term(Term) -->
	{ functor(Term, Name, Arity),		% Validates Term as callable
	  prolog_load_context(module, Module)
	},
	[ :- dynamic(Name/Arity),
	  db:db_term(Module, Term)
	].
	
:- multifile
	user:term_expansion/2.

user:term_expansion((:- db_term(Spec)), Clauses) :-
	phrase(compile_db_term(Spec), Clauses).


		 /*******************************
		 *	      ATTACH		*
		 *******************************/

%%	attach_db(:File, +Options)
%	
%	Use File as persistent database  for   the  calling  module. The
%	calling module must defined db_term/1   to  declare the database
%	terms.  Defined options:
%	
%		* sync(+Sync)
%		One of =close= (close journal after write), =flush=
%		(default, flush journal after write) or =none=
%		(handle as fully buffered stream).

db_attach(Spec, Options) :-
	strip_module(Spec, Module, File),
	db_set_options(Module, Options),
	db_attach_file(Module, File).

db_set_options(Module, Options) :-
	retractall(db_option(Module, _)),
	option(sync(Sync), Options, flush),
	must_be(oneof([close,flush,none]), Sync),
	assert(db_option(Module, sync(Sync))).

db_attach_file(Module, File) :-
	db_file(Module, Old, _), !,		% we already have a db
	(   Old == File
	->  true
	;   permission_error(attach, db, File)
	).
db_attach_file(Module, File) :-
	db_load(Module, File), !.
db_attach_file(Module, File) :-
	assert(db_file(Module, File, 0)).

db_load(Module, File) :-
	retractall(db_file(Module, _, _)),
	catch(open(File, read, In, [encoding(utf8)]), _, fail), !,
	debug(db, 'Loading database ~w', [File]),
	call_cleanup((read_action(In, T0),
		      load_db(T0, In, Module)),
		     close(In)),
	debug(db, 'Loaded ~w', [File]),
	time_file(File, Modified),
	assert(db_file(Module, File, Modified)).

load_db(end_of_file, _, _) :- !.
load_db(assert(Term), In, Module) :-
	db_term(Module, Term), !,
	assert(Module:Term),
	read_action(In, T1),
	load_db(T1, In, Module).
load_db(retractall(Term), In, Module) :-
	db_term(Module, Term), !,
	retractall(Module:Term),
	set_dirty(Module),
	read_action(In, T1),
	load_db(T1, In, Module).
load_db(Term, In, Module) :-
	print_message(error, illegal_term(Term)),
	read_action(In, T1),
	load_db(T1, In, Module).

db_clean(Module) :-
	retractall(db_dirty(Module)),
	(   db_term(Module, Term),
	    retractall(Module:Term),
	    fail
	;   true
	).

%%	db_assert(:Term) is det.
%	
%	Assert Term into the database  and   record  it for persistency.
%	Note that if the on-disk file  has   been  modified  it is first
%	reloaded.

db_assert(Spec) :-
	strip_module(Spec, Module, Term),
	assert(Module:Term),
	persistent(Module, assert(Term)).

persistent(Module, Action) :-
	(   db_stream(Module, Stream)
	->  true
	;   db_file(Module, File, _Modified)
	->  db_sync(Module, reload),		% Is this correct?
	    open(File, append, Stream,
		 [ close_on_abort(false),
		   encoding(utf8),
		   lock(write)
		 ]),
	    assert(db_stream(Module, Stream))
	;   existence_error(db_file, Module)
	),
	write_action(Stream, Action),
	sync(Module, Stream).

%%	sync(+Module, +Stream) is det.
%
%	Synchronise journal after a write.   Using  =close=, the journal
%	file is closed, making it easier   to  edit the file externally.
%	Using =flush= flushes the stream  but   does  not close it. This
%	provides better performance. Using  =none=,   the  stream is not
%	even flushed. This makes the journal   sensitive to crashes, but
%	much faster.

sync(Module, Stream) :-
	db_option(Module, sync(Sync)),
	(   Sync == close
	->  db_sync(Module, close)
	;   Sync == flush
	->  flush_output(Stream)
	;   true
	).

read_action(Stream, Action) :-
	read_term(Stream, Action, [module(db)]).

write_action(Stream, Action) :-
	\+ \+ ( numbervars(Action, 0, _, [singletons(true)]),
		format(Stream, '~W.~n',
		       [ Action,
			 [ quoted(true),
			   numbervars(true),
			   module(db)
			 ]
		       ])
	      ).

%%	db_retractall(:Term) is det.
%
%	Retract all matching facts and do the   same in the database. If
%	Term is unbound, db_term/1 from the   calling  module is used as
%	generator.
%	
%	@tbd	Only flag dirty if clauses are deleted.

db_retractall(Spec) :-
	strip_module(Spec, Module, Term),
	(   var(Term)
	->  forall(db_term(Module, Term),
		   db_retractall(Module:Term))
	;   retractall(Module:Term),
	    set_dirty(Module),
	    persistent(Module, retractall(Term))
	).


set_dirty(Module) :-
	(   db_dirty(Module)
	->  true
	;   assert(db_dirty(Module))
	).

%%	db_sync(?What)
%
%	Synchronise database with the associated file.  What is one of:
%	
%		* reload
%		Database is reloaded from file
%		* gc
%		Database was re-written, deleting all retractall
%		statements.
%		* close
%		Database stream was closed
%		* nop
%		No-operation performed
%		
%	With unbound What, db_sync/1 will reload  the database if it was
%	modified on disk, gc it if it  is   dirty  and close it if it is
%	opened.

db_sync(Spec) :-
	strip_module(Spec, Module, What),
	db_sync(Module, What).


db_sync(Module, reload) :-
	\+ db_stream(Module, _),		% not open
	db_file(Module, File, ModifiedWhenLoaded),
	catch(time_file(File, Modified), _, fail),
	Modified > ModifiedWhenLoaded, !,	% Externally modified
	debug(db, 'Database ~w was externally modified; reloading', [File]),
	db_clean(Module),
	db_load(Module, File).
db_sync(Module, gc) :-
	db_dirty(Module), !,
	db_sync(Module, close),
	db_file(Module, File, Modified),
	atom_concat(File, '.new', NewFile),
	debug(db, 'Database ~w is dirty; cleaning', [File]),
	open(NewFile, write, Out, [encoding(utf8)]),
	(   db_term(Module, Term),
	    Module:Term,
	    write_action(Out, assert(Term)),
	    fail
	;   true
	),
	close(Out),
	retractall(db_file(Module, File, Modified)),
	rename_file(NewFile, File),
	time_file(File, NewModified),
	assert(db_file(Module, File, NewModified)).
db_sync(Module, close) :-
	retract(db_stream(Module, Stream)), !,
	db_file(Module, File, _),
	debug(db, 'Database ~w is open; closing', [File]),
	close(Stream),
	time_file(File, Modified),
	retractall(db_file(Module, File, _)),
	assert(db_file(Module, File, Modified)).
db_sync(_, nop) :- !.
db_sync(_, _).


%%	db_sync_all(+What)
%
%	Sync all registered databases.

db_sync_all(What) :-
	must_be(oneof([reload,gc,close]), What),
	forall(db_file(Module, _, _),
	       db_sync(Module:What)).


		 /*******************************
		 *	       CLOSE		*
		 *******************************/

close_dbs :-
	forall(db_stream(_Module, Stream),
	       close(Stream)).

:- at_halt(close_dbs).


