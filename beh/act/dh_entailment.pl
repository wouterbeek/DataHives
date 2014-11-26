:- module(
  dh_entailment,
  [
    deductions/1, % ?Deductions:nonneg
    deductive_action/1, % +DirectedTriple:compound
    evaluate_entailment/0
  ]
).

/** <module> DataHives entailment

DataHives agents that implement RDFS 1.1 entailment.

@author Wouter Beek
@version 2014/06-2014/09
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdfs_build2)).
:- use_module(plRdf(entailment/rdf_bnode_map)).
:- use_module(plRdf(entailment/rdf_entailment_patterns)).

:- use_module(dh(agent/dh_agent)).
:- use_module(dh(agent/dh_agent_create)).
:- use_module(dh(agent/dh_agent_property_local)).
:- use_module(dh(core/dh_cycle)). % agent_property(cycles)
:- use_module(dh(core/dh_generics)).
:- use_module(dh(core/dh_messages)).

%! number_of_deductions(?Deductions:nonneg) is nondet.

:- thread_local(number_of_deductions/1).

:- initialization(init).



%! deductive_action(+DirectedTriple:compound) is det.

deductive_action(DirTriple):-
  directed_triple(DirTriple, Triple),
  forall(
    rdf_entailment_pattern_match(Triple, EntailedTriple),
    rdf_assert_entailment(EntailedTriple)
  ).


evaluate_entailment:-
  number_of_deductions(Deductions),
  cycles(Lifetime),
  Fitness is Deductions / Lifetime,
  (   Fitness < 0.5
  -> thread_exit(done)
  ;  true
  ),
  print_message(
    informational,
    entailment_fitness(Deductions,Lifetime,Fitness)
  ).


%! rdf_assert_entailment(+Triple:compound) is det.

rdf_assert_entailment(rdf(U1,V,W)):-
  % Literals that are not yet replaced by blank nodes
  %  are added to the blank node map.
  (   rdf_is_literal(U1)
  ->  term_set_bnode(user, U1, U2)
  ;   U2 = U1
  ),

  % Assert the triple in the graph named after the agent alias.
  graph_self(MyGraph),
  rdf_assert(U2, V, W, MyGraph),

  % Count the number of deductions per agent.
  increment_deductions.


%! rdf_entailment_pattern_match(
%!   +Predicate1:compound,
%!   -Conclusion:compound
%! ) is det.

rdf_entailment_pattern_match(Premise1, rdf(U1,V,W)):-
  % Instantiate the predicates of an entailment pattern.
  (
    % One predicate, one conclusion.
    rdf_entailment_pattern(Premise1, rdf(U1,V,W))
  ;
    % Two predicates, one conclusion.
    rdf_entailment_pattern(Premise1, rdf(X,Y,Z), rdf(U1,V,W)),
    % The second premise must be true in cache.
    rdf(X, Y, Z)
  ),

  % Use the existing blank node map
  %  to replace literals with their blank node proxy.
  (   rdf_is_literal(U1),
      term_get_bnode(user, U1, U2)
  ->  true
  ;   U2 = U1
  ),

  % The conclusion must be new.
  \+ rdf(U2, V, W).



% AGENT PROPERTIES

deductions(Deductions):-
  number_of_deductions(Deductions), !.
deductions(0).

init:-
  rdfs_assert_property(
    dho:deductions,
    dho:agentPropertyLocal,
    dho:'Agent',
    xsd:nonNegativeInteger,
    deductions,
    'The number of deductions that have made by an agent \c
     since it was created.',
    dho
  ).


%! increment_deductions is det.
% Increments the number of deductions produced by an agent.

increment_deductions:-
  increment_deductions(1).

increment_deductions(N):-
  (
    retract(number_of_deductions(N1)), !
  ;
    N1 = 0
  ),
  N2 is N1 + N,
  assert(number_of_deductions(N2)).


%! reset_number_of_deductions is det.

reset_number_of_deductions:-
  retractall(number_of_deductions(_)),
  assert(number_of_deductions(0)).



% MESSAGES

:- multifile(prolog:message//1).

prolog:message(entailment_fitness(Deductions,Lifetime,Fitness)) -->
  {graph_self(MyGraph)},
  ['[~w] ~f (~D entailments; ~D steps)'-[MyGraph,Fitness,Deductions,Lifetime]].

