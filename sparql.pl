/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

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


:- module(sparql,
	  [ sparql_query/3,		% +Query, -Result, +Options
	    sparql_compile/3,		% +Query, -Compiled, +Options
	    sparql_run/2		% +Compiled, -Reply
	  ]).
:- use_module(library(option)).
:- use_module(sparql_grammar).
:- use_module(sparql_runtime).
:- use_module(rdfql_util).

:- multifile
	function/2.			% user-defined functions

%%	sparql_query(+Query, -Reply, +Options)
%	
%	Where Query is either a SPARQL query text or a parsed
%	query.  Reply depends on the type of query:
%	
%		|SELECT		| row(Col1, Col2, ....) |
%		|CONSTRUCT	| rdf(S,P,O) |
%		|DESCRIBE	| rdf(S,P,O) |
%		|ASK		| Reply == true or failure of pred |
%		
%	Options are:
%	
%		* entailment(Entailment)
%		Specify the entailment module used (default: rdf)
%		
%		* base_uri(Base)
%		Specify the base IRI to use for parsing the query
%		
%		* type(-Type)
%		Returns one of select(-VarNames), construct, describe or
%		ask.
%		
%		* ordered(-Bool)
%		True if query contains an ORDER BY clause
%		
%		* distinct(-Bool)
%		True if query contains a DISTINCT clause

sparql_query(Query, Reply, Options) :-
	sparql_compile(Query, Compiled, Options),
	sparql_run(Compiled, Reply).


%%	sparql_compile(+Query, -Compiled, +Options)
%	
%	Performs  the  compilation  pass  of  solving  a  SPARQL  query.
%	Splitting serves two purposes. The result of the compilation can
%	be cached if desired and through  Options we can get information
%	about the parsed query.

sparql_compile(Query, sparql_query(Parsed, ReplyTemplate, Module), Options) :-
	sparql_parse(Query, Parsed, Options),
	option(entailment(Entailment), Options, rdf),
	option(type(Type), Options, _),
	option(ordered(Order), Options, _),
	option(distinct(Distinct), Options, _),
	entailment_module(Entailment, Module),
	prepare(Parsed, Type, Order, Distinct, ReplyTemplate).

prepare(select(Vars, _, _, S), select(Names), O, D, Reply) :- !,
	select_result(Vars, Reply, Names),
	solutions(S, O, D).
prepare(construct(_,_,_,S), construct, O, D, _) :- !,
	solutions(S, O, D).
prepare(ask(_,_), ask, false, false, _) :- !.
prepare(describe(_,_,_,S), describe, O, D, _) :- !,
	solutions(S, O, D).
prepare(Query, Type, _, _, _) :-
	nonvar(Type),
	functor(Type, Expected, _),
	functor(Query, Found, _),
	throw(error(type_error(query_type(Expected), Found), _)).

solutions(distinct(S), O, true) :- !,
	solutions(S, O).
solutions(S, O, false) :-
	solutions(S, O).

solutions(solutions(unsorted, _, _), O) :- !,
	O = false.
solutions(_, true).

%%	sparql_run(+Compiled, -Reply) is nondet.
%	
%	Runs a compiled SPARQL query, returning the result incrementally
%	on backtracking. Provided there are  no   errors  in  the SPARQL
%	implementation  the  only   errors   this    can   produce   are
%	resource-related errors.

sparql_run(sparql_query(Parsed, Reply, Module), Reply) :-
	   sparql_run(Parsed, Reply, Module).

sparql_run(select(_Vars, _DataSets, Query, Solutions), Reply, Module) :-
	select_results(Solutions, Reply, Module:Query).
sparql_run(construct(Triples, _DataSets, Query, Solutions), Reply, Module) :-
	select_results(Solutions, Reply,
		       (   Module:Query,
			   member(Reply, Triples)
		       )).
sparql_run(ask(_DataSets, Query), Result, Module) :-
	(   Module:Query
	->  Result = true
	;   Result = false
	).
sparql_run(describe(IRIs, _DataSets, Query, Solutions), Reply, Module) :-
	select_results(Solutions, Reply,
		       (   Module:Query,
			   member(IRI, IRIs)
		       )),
	sparql_describe(IRI, Module, Reply).

	
%%	select_results(+Spec, -Reply, :Goal)
%	
%	Apply ordering and limits on result-set.

select_results(distinct(solutions(Order, Limit, Offset)), Reply, Goal) :- !,
	select_results(distinct, Offset, Limit, Order, Reply, Goal).
select_results(solutions(Order, Limit, Offset), Reply, Goal) :-
	select_results(all, Offset, Limit, Order, Reply, Goal).


%%	select_result(+Bindings, -Row, -Names) is det.
%	
%	Transform the list Bindings of the form Name=Var into a Row term
%	of the form row(Col1, Col2, ...) and a list of column-names. For
%	example:
%	
%	==
%	?- select_result([x=1,y=2], Row, Names).
%	Row = row(1,2), Names = [x,y]
%	==

select_result(Bindings, Row, Names) :-
	vars_in_bindings(Bindings, Vars, Names),
	Row =.. [row|Vars].

vars_in_bindings([], [], []).
vars_in_bindings([Name=Var|T0], [Var|T], [Name|NT]) :-
	vars_in_bindings(T0, T, NT).

%%	sparql_describe(+IRI, -Triple)
%	
%	Return  -on  backtracking-  triples  that    describe  IRI.  The
%	documentation does not specify which   triples  must be returned
%	for a description. As a way to  get started we simply return all
%	direct properties.

sparql_describe(IRI, Module, rdf(IRI, P, O)) :-
	Module:rdf(IRI, P, O).
