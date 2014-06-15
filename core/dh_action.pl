:- module(
  dh_action,
  [
    deductions/1, % ?Deductions:nonneg
    default_action/4, % +From:or([bnode,iri,literal])
                      % -Direction:oneof([backward,forward])
                      % -Link:iri
                      % -To:or([bnode,iri,literal])
    deductive_action/4, % +From:or([bnode,iri,literal])
                        % -Direction:oneof([backward,forward])
                        % -Link:iri
                        % -To:or([bnode,iri,literal])
    lifetime/1, % ?Lifetime:nonneg
    forbide_path/1 % +From:or([bnode,iri,literal])
  ]
).

/** <module> DataHives actions

Action predicates for agents in DataHives.

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/02, 2014/04, 2014/06
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).

:- use_module(plRdf(rdf_name)).
:- use_module(plRdf_ent(rdf_bnode_map)).
:- use_module(plRdf_ent(rdf_entailment_patterns)).
:- use_module(plRdf_term(rdf_term)).

:- use_module(dh_core(dh_communication)).
:- use_module(dh_core(dh_navigation)).
:- use_module(dh_test(dh_test)).

%! deductions(?Deductions:nonneg) is nondet.

:- thread_local(deductions/1).

%! lifetime(?Lifetime:nonneg) is nondet.

:- thread_local(lifetime/1).



default_action(From, Dir, Link, To):-
  dir_trans(Dir, Orient),
  dcg_with_output_to(atom(Arrow), arrow(Orient, 4)),
  dcg_with_output_to(atom(Triple), rdf_triple_name(From, Link, To)),
  debug(dh, '~w\t~w', [Arrow,Triple]),
  increment_lifetime.


deductive_action(From, backward, Link, To):- !,
  deductive_action(To, forward, Link, From).
deductive_action(From, forward, Link, To):-
  forall(
    rdf_entailment_pattern_match(From, Link, To, U1, V, W),
    rdf_assert_entailment(U1, V, W)
  ),
  increment_lifetime.


rdf_assert_entailment(U1, V, W):-
  (
    rdf_is_literal(U1)
  ->
    r2b(user, U1, U2)
  ;
    U2 = U1
  ),
  thread_self(Me),
  thread_property(Me, alias(MyName)),
  rdf_assert(U2, V, W, MyName),
  increment_deductions.


rdf_entailment_pattern_match(From, Link, To, U1, V, W):-
  (
    rdf_entailment_pattern(rdf(From,Link,To), rdf(U1,V,W))
  ;
    rdf_entailment_pattern(rdf(From,Link,To), rdf(X,Y,Z), rdf(U1,V,W)),
    rdf(X, Y, Z)
  ),
  (
    rdf_is_literal(U1),
    has_r2b(user, U1, U2)
  ->
    true
  ;
    U2 = U1
  ),
  \+ rdf(U2, V, W).


%! increment_deductions is det.
% Increments the number of deductions produced by an agent.

increment_deductions:-
  (
    retract(deductions(Deductions1)), !
  ;
    Deductions1 = 0
  ),
  succ(Deductions1, Deductions2),
  assert(deductions(Deductions2)).


%! increment_lifetime is det.
% Increments an agent's liftime, e.g. a single step.

increment_lifetime:-
  (
    retract(lifetime(Lifetime1)), !
  ;
    Lifetime1 = 0
  ),
  succ(Lifetime1, Lifetime2),
  assert(lifetime(Lifetime2)).

forbide_path(From):-
  single_alley(From,Alley),
  devalue(Alley).

single_alley(Next, [[Prev,Link,Next]|Alley]):-
  single_step(Prev, Link, Next),
  single_alley(Prev, Alley).
single_alley(_, []).

single_step(Prev, Link, Next):-
  rdf(Prev, Link, Next),
  \+ ((
    rdf(Prev, Link0, Next0),
    Link0 \== Link,
    Next0 \== Next
  )).

% Value of triple S-P-O decreased by one
%
devalue([]):-!.
devalue([H|T]):-
  devalueTriple(H),
  devalue(T).
devalueTriple([S,P,O]):-
  edge_count(S,P,O,Count),
  succ(NextValue,Count),  % Should we use -2 instead of -1 ?
  edge_count(S,P,O,NextValue).

dir_trans(backward, left).
dir_trans(forward, right).

