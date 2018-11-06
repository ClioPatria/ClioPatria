/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam
                              VU University Amsterdam
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

:- module(sparql,
          [ sparql_query/3,             % +Query, -Result, +Options
            sparql_compile/3,           % +Query, -Compiled, +Options
            sparql_run/2                % +Compiled, -Reply
          ]).
:- use_module(library(option)).
:- use_module(library(assoc)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db), [rdf_is_bnode/1]).
:- use_module(library(semweb/rdf_optimise)).
:- use_module(library(settings)).
:- use_module(sparql_grammar).
:- use_module(sparql_runtime).
:- use_module(rdfql_util).
:- use_module(library(settings)).
:- include(entailment(load)).

:- multifile
    function/2.                     % user-defined functions

:- setting(entailment, atom, rdf,
           'Default entailment used for SPARQL queries').

%!  sparql_query(+Query, -Reply, +Options)
%
%   Where Query is either a SPARQL query text or a parsed
%   query.  Reply depends on the type of query:
%
%           |SELECT         | row(Col1, Col2, ....) |
%           |CONSTRUCT      | rdf(S,P,O) |
%           |DESCRIBE       | rdf(S,P,O) |
%           |ASK            | Reply == true or failure of pred |
%
%   Options are:
%
%           * entailment(Entailment)
%           Specify the entailment module used.  The default is
%           controlled by the setting =|sparql:entailment|=.
%
%           * base_uri(Base)
%           Specify the base IRI to use for parsing the query
%
%           * type(-Type)
%           Returns one of select(-VarNames), construct, describe or
%           ask.
%
%           * ordered(-Bool)
%           True if query contains an ORDER BY clause
%
%           * distinct(-Bool)
%           True if query contains a DISTINCT clause

sparql_query(Query, Reply, Options) :-
    sparql_compile(Query, Compiled, Options),
    sparql_run(Compiled, Reply).


%!  sparql_compile(+Query, -Compiled, +Options)
%
%   Performs  the  compilation  pass  of  solving  a  SPARQL  query.
%   Splitting serves two purposes. The result of the compilation can
%   be cached if desired and through  Options we can get information
%   about the parsed query.

sparql_compile(Query, sparql_query(Optimised, ReplyTemplate, Module), Options) :-
    sparql_parse(Query, Parsed, Options),
    optimise(Parsed, Optimised, Options),
    (   option(entailment(Entailment), Options)
    ->  true
    ;   setting(entailment, Entailment)
    ),
    option(type(Type), Options, _),
    option(ordered(Order), Options, _),
    option(distinct(Distinct), Options, _),
    entailment_module(Entailment, Module),
    prepare(Parsed, Type, Order, Distinct, ReplyTemplate).

prepare(select(Vars, _, _, S), select(Names), O, D, Reply) :-
    !,
    select_result(Vars, Reply, Names),
    solutions(S, O, D).
prepare(construct(_,_,_,S), construct, O, D, _) :-
    !,
    solutions(S, O, D).
prepare(ask(_,_,S), ask, O, D, _) :-
    !,
    solutions(S, O, D).
prepare(describe(_,_,_,S), describe, O, D, _) :-
    !,
    solutions(S, O, D).
prepare(update(_), update, false, false, _) :- !.
prepare(Query, Type, _, _, _) :-
    nonvar(Type),
    functor(Type, Expected, _),
    functor(Query, Found, _),
    throw(error(type_error(query_type(Expected), Found), _)).

solutions(distinct(S), O, true) :-
    !,
    solutions(S, O).
solutions(S, O, false) :-
    solutions(S, O).

solutions(solutions(_Group, _Having, _Aggregate, unsorted, _, _), O) :-
    !,
    O = false.
solutions(_, true).


%!  optimise(+Parsed, -Optimised, +Options) is det.
%
%   Perform  sparql  query   optimization    using   rdf_optimise/2.
%   Currently,  UPDATE  requests  are  not   optimized.
%
%   @tbd The UPDATE modify requests involve a query and must be
%   optimized.

optimise(update(Updates), update(Updates), _) :- !.
optimise(Parsed, Optimised, Options) :-
    (   option(optimise(Optimise), Options)
    ->  Optimise == true
    ;   setting(cliopatria:optimise_query, true)
    ),
    prolog_goal(Parsed, Goal0),
    simplify_group(Goal0, Goal1),
    optimise_eval(Goal1, Goal2),
    rdf_optimise(Goal2, Goal3),
    !,
    bind_null(Goal3, Goal, Options),
    set_prolog_goal(Parsed, Goal, Optimised).
optimise(Parsed, Optimised, Options) :-
    prolog_goal(Parsed, Goal0),
    simplify_group(Goal0, Goal1),
    bind_null(Goal1, Goal, Options),
    set_prolog_goal(Parsed, Goal, Optimised).

% remove the outer SPARQL group. It has no meaning and reduces
% readability.

simplify_group(sparql_group(G), G) :- !.
simplify_group(sparql_group(G, VIn, VOut), G) :-
    VIn = VOut,
    !.
simplify_group(Goal, Goal).

bind_null(Goal0, Goal, Options) :-
    option(bind_null(true), Options),
    !,
    serql_select_bind_null(Goal0, Goal).
bind_null(Goal, Goal, _).


prolog_goal(select(_Proj, _DataSets, Goal, _Solutions), Goal).
prolog_goal(construct(_Templ, _DataSets, Goal, _Solutions), Goal).
prolog_goal(ask(_DataSets, Goal, _Solutions), Goal).
prolog_goal(describe(_Proj, _DataSets, Goal, _Solutions), Goal).
prolog_goal(sparql_group(Goal), Goal).
prolog_goal(sparql_group(Goal,_VA,_VZ), Goal).

set_prolog_goal(select(Proj, DataSets, _Goal, Solutions), Goal,
                select(Proj, DataSets, Goal, Solutions)).
set_prolog_goal(construct(Templ, DataSets, _Goal, Solutions), Goal,
                construct(Templ, DataSets, Goal, Solutions)).
set_prolog_goal(ask(DataSets, _Goal, Solutions), Goal,
                ask(DataSets, Goal, Solutions)).
set_prolog_goal(describe(Proj, DataSets, _Goal, Solutions), Goal,
                describe(Proj, DataSets, Goal, Solutions)).
set_prolog_goal(sparql_group(_Goal), Goal, Goal).
set_prolog_goal(sparql_group(_Goal,VA,VZ), Goal, (Goal,VA=VZ)).


%!  optimise_eval(+Goal0, -Goal) is det.
%
%   Perform partial evaluation on   sparql_true/1  and sparql_eval/2
%   goals.

optimise_eval(GoalIn, GoalOut) :-
    annotate_variables(GoalIn, Vars),
    optimise_annotated(GoalIn, GoalOut),
    unbind_variables(Vars).

%!  annotate_variables(+Goal, -Vars) is det.
%
%   Annotate variables that appear in  Goal.   The  annotation  is a
%   variable attribute named =annotations=  and   the  value of this
%   attribute is a list of annotations.

annotate_variables(Goal, Vars) :-
    empty_assoc(Vars0),
    annotate_vars(Goal, Vars0, Vars).

annotate_vars(Var, _, _) :-
    var(Var),
    !,
    instantiation_error(Var).
annotate_vars((A,B), Vars0, Vars) :-
    !,
    annotate_vars(A, Vars0, Vars1),
    annotate_vars(B, Vars1, Vars).
annotate_vars((A;B), Vars0, Vars) :-
    !,
    annotate_vars(A, Vars0, Vars1),
    annotate_vars(B, Vars1, Vars).
annotate_vars((A*->B), Vars0, Vars) :-
    !,
    annotate_vars(A, Vars0, Vars1),
    annotate_vars(B, Vars1, Vars).
annotate_vars(sparql_group(G), Vars0, Vars) :-
    !,
    annotate_vars(G, Vars0, Vars).
annotate_vars(sparql_group(G, _, _), Vars0, Vars) :-
    !,
    annotate_vars(G, Vars0, Vars).
annotate_vars(rdf(S,P,_), Vars0, Vars) :-
    !,
    annotate_var(S, resource, Vars0, Vars1),
    annotate_var(P, resource, Vars1, Vars).
annotate_vars(rdf(S,P,_,G), Vars0, Vars) :-
    !,
    annotate_var(S, resource, Vars0, Vars1),
    annotate_var(P, resource, Vars1, Vars2),
    annotate_var(G, resource, Vars2, Vars).
annotate_vars(_, Vars, Vars).

annotate_var(V, Type, Vars0, Vars) :-
    var(V),
    (   get_attr(V, annotations, A0)
    ->  \+ memberchk(Type, A0)
    ;   A0 = []
    ),
    !,
    put_attr(V, annotations, [Type|A0]),
    put_assoc(V, Vars0, true, Vars).
annotate_var(_, _, Vars, Vars).

unbind_variables(VarAssoc) :-
    assoc_to_keys(VarAssoc, VarList),
    maplist(unbind_var, VarList).

unbind_var(V) :-
    del_attr(V, annotations).

%!  optimise_eval(+GoalIn, -GoalOut)

optimise_annotated((A0,B0), (A,B)) :-
    !,
    optimise_annotated(A0, A),
    optimise_annotated(B0, B).
optimise_annotated((A0;B0), (A;B)) :-
    !,
    optimise_annotated(A0, A),
    optimise_annotated(B0, B).
optimise_annotated((A0*->B0), (A*->B)) :-
    !,
    optimise_annotated(A0, A),
    optimise_annotated(B0, B).
optimise_annotated(sparql_group(G0), sparql_group(G)) :-
    !,
    optimise_annotated(G0, G).
optimise_annotated(sparql_group(G0, OV, IV), sparql_group(G, OV, IV)) :-
    !,
    optimise_annotated(G0, G).
optimise_annotated(sparql_true(E), G) :-
    !,
    sparql_simplify(sparql_true(E), G).
optimise_annotated(sparql_eval(E,V), G) :-
    !,
    sparql_simplify(sparql_eval(E,V), G).
optimise_annotated(G, G).


%!  sparql_run(+Compiled, -Reply) is nondet.
%
%   Runs a compiled SPARQL query, returning the result incrementally
%   on backtracking. Provided there are  no   errors  in  the SPARQL
%   implementation  the  only   errors   this    can   produce   are
%   resource-related errors.

sparql_run(sparql_query(Parsed, Reply, Module), Reply) :-
    sparql_reset_bnodes,
    sparql_run(Parsed, Reply, Module).

sparql_run(select(_Vars, _DataSets, Query, Solutions), Reply, Module) :-
    select_results(Solutions, Reply, Module:Query).
sparql_run(construct(Triples, _DataSets, Query, Solutions), Reply, Module) :-
    select_results(Solutions, Reply,
                   Module:( Query,
                            rdfql_triple_in(Reply, Triples)
                          )).
sparql_run(ask(_DataSets, Query, _Solutions), Result, Module) :-
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
sparql_run(update(Updates), Result, Module) :-
    (   Module:sparql_update(Updates)
    ->  Result = true
    ;   Result = false
    ).

%!  select_results(+Spec, -Reply, :Goal)
%
%   Apply ordering and limits on result-set.
%
%   @tbd    Handle =reduced=

:- meta_predicate select_results(+,+,0).
:- public select_results/3.             % used on sparql_subquery/4

select_results(distinct(solutions(Group, Having, Agg, Order, Limit, Offset)),
               Reply, Goal) :-
    !,
    select_results(distinct, Group, Having, Agg, Offset, Limit,
                   Order, Reply, Goal).
select_results(reduced(Solutions),
               Reply, Goal) :-
    !,
    select_results(Solutions, Reply, Goal).
select_results(solutions(Group, Having, Agg, Order, Limit, Offset),
               Reply, Goal) :-
    select_results(all, Group, Having, Agg, Offset, Limit,
                   Order, Reply, Goal).


%!  select_result(+Bindings, -Row, -Names) is det.
%
%   Transform the list Bindings of the form Name=Var into a Row term
%   of the form row(Col1, Col2, ...)   and a term names(Name1, ...).
%   For example:
%
%   ==
%   ?- select_result([x=1,y=2], Row, Names).
%   Row = row(1,2), Names = names(x,y)
%   ==

select_result(Bindings, Row, Names) :-
    vars_in_bindings(Bindings, Vars, VarNames),
    Names =.. [names|VarNames],
    Row =.. [row|Vars].

vars_in_bindings([], [], []).
vars_in_bindings([Name=Var|T0], [Var|T], [Name|NT]) :-
    vars_in_bindings(T0, T, NT).

%!  sparql_describe(+IRI, -Triple)
%
%   Return  -on  backtracking-  triples  that    describe  IRI.  The
%   documentation does not specify which   triples  must be returned
%   for a description. As a way to  get started we simply return all
%   direct properties.

sparql_describe(_Var=IRI, Module, Triple) :-
    !,
    sparql_describe(IRI, Module, Triple).
sparql_describe(IRI, Module, Triple) :-
    empty_assoc(Seen),
    sparql_describe(IRI, Module, Triple, Seen).

sparql_describe(IRI, Module, Triple, Seen) :-
    Module:rdf(IRI, P, O),
    (   rdf_is_bnode(O),
        \+ get_assoc(O, Seen, true)
    ->  (   Triple = rdf(IRI, P, O)
        ;   put_assoc(O, Seen, true, Seen2),
            sparql_describe(O, Module, Triple, Seen2)
        )
    ;   Triple = rdf(IRI, P, O)
    ).
