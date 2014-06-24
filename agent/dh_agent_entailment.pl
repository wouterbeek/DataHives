:- module(
  dh_agent_entailment,
  [
    deductions/1, % ?Deductions:nonneg
    deductive_action/4, % +From:or([bnode,iri,literal])
                        % -Direction:oneof([backward,forward])
                        % -Link:iri
                        % -To:or([bnode,iri,literal])
    evaluate_entailment/0
  ]
).

/** <module> DataHives: entailment agents

DataHives agents that implement RDFS 1.1 entailment.

@author Wouter Beek
@version 2014/06
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf_ent(rdf_bnode_map)).
:- use_module(plRdf_ent(rdf_entailment_patterns)).

:- use_module(dh_core(dh_cycle)).

%! deductions(?Deductions:nonneg) is nondet.

:- thread_local(deductions/1).



%! deductive_action(
%!   +From:or([bnode,iri,literal]),
%!   -Direction:oneof([backward,forward]),
%!   -Link:iri,
%!   -To:or([bnode,iri,literal])
%! ) is det.

deductive_action(From, backward, Link, To):- !,
  deductive_action(To, forward, Link, From).
deductive_action(From, forward, Link, To):-
  forall(
    rdf_entailment_pattern_match(rdf(From,Link,To), rdf(U1,V,W)),
    rdf_assert_entailment(rdf(U1,V,W))
  ).


evaluate_entailment:-
  deductions(Deductions),
  number_of_cycles(Lifetime),
  Fitness is Deductions / Lifetime,
  (
    Fitness < 0.5
  ->
    thread_exit(done)
  ;
    true
  ),
  print_message(
    informational,
    entailment_fitness(Deductions,Lifetime,Fitness)
  ).


%! rdf_assert_entailment(+Triple:compound) is det.

rdf_assert_entailment(rdf(U1,V,W)):-
  % Literals that are not yet replaced by blank nodes
  % are added to the blank node map here.
  (
    rdf_is_literal(U1)
  ->
    r2b(user, U1, U2)
  ;
    U2 = U1
  ),
  
  % Assert the triple in the graph named after the agent alias.
  thread_self(Me),
  thread_property(Me, alias(MyName)),
  rdf_assert(U2, V, W, MyName),
  
  % Count the number of deductions per agent.
  increment_deductions.


%! rdf_entailment_pattern_match(
%!   +Predicate1:compound,
%!   -Conclusion:compound
%! ) is det.

rdf_entailment_pattern_match(rdf(From,Link,To), rdf(U1,V,W)):-
  % Instantiate the predicates of an entailment pattern.
  (
    % One predicate, one conclusion.
    rdf_entailment_pattern(rdf(From,Link,To), rdf(U1,V,W))
  ;
    % Two predicates, one conclusion.
    rdf_entailment_pattern(rdf(From,Link,To), rdf(X,Y,Z), rdf(U1,V,W)),
    % The second predicate must be in cache.
    rdf(X, Y, Z)
  ),
  
  % Use the existing blank node map
  % to replace literals with their blank node proxy.
  (
    rdf_is_literal(U1),
    has_r2b(user, U1, U2)
  ->
    true
  ;
    U2 = U1
  ),
  
  % The conclusion must be new.
  \+ rdf(U2, V, W).


%! increment_deductions is det.
% Increments the number of deductions produced by an agent.

increment_deductions:-
  (
    retract(deductions(N1)), !
  ;
    N1 = 0
  ),
  N2 is N1 + 1,
  assert(deductions(N2)).



% Messages

:- multifile(prolog:message//1).

prolog:message(entailment_fitness(Deductions,Lifetime,Fitness)) -->
  {
    thread_self(Me),
    thread_property(Me, alias(MyName))
  },
  ['[~w] ~f (~D entailments; ~D steps)'-[MyName,Fitness,Deductions,Lifetime]].

