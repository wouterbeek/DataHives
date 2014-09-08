:- module(
  dh_entailment,
  [
    deductive_action/1, % +DirectedTriple:compound
    deductive_exit/1, % +Initialization:compound
    evaluate_entailment/0,
% Statistics
    dh_agent_deductions/1 % ?NumberOfDeductions:nonneg
  ]
).

/** <module> DataHives entailment

DataHives agents that implement RDFS 1.1 entailment.

@author Wouter Beek
@version 2014/06-2014/08
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf_ent(rdf_bnode_map)).
:- use_module(plRdf_ent(rdf_entailment_patterns)).

:- use_module(dh_agent(dh_agent_create)). % @tbd Relocate?
:- use_module(dh_agent(dh_agent_property)).
:- use_module(dh_core(dh_cycle)).
:- use_module(dh_core(dh_generics)).

%! number_of_deductions(?Deductions:nonneg) is nondet.

:- thread_local(number_of_deductions/1).



%! deductive_action(+DirectedTriple:compound) is det.

deductive_action(DirTriple):-
  directed_triple(DirTriple, Triple),
  forall(
    rdf_entailment_pattern_match(Triple, EntailedTriple),
    rdf_assert_entailment(EntailedTriple)
  ).


%! deductive_exit(+Initialization:compound) is det.
% @tbd Test of agent definition-specific exit conditions.

deductive_exit(Init):-
  default_exit(Init).


evaluate_entailment:-
  number_of_deductions(Deductions),
  dh_agent_cycles(Lifetime),
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
    term_to_bnode(user, U1, U2)
  ;
    U2 = U1
  ),

  % Assert the triple in the graph named after the agent alias.
  dh_agent_graph(MyGraph),
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
  % to replace literals with their blank node proxy.
  (
    rdf_is_literal(U1),
    term_to_bnode(user, U1, U2)
  ->
    true
  ;
    U2 = U1
  ),

  % The conclusion must be new.
  \+ rdf(U2, V, W).



% Statistics

%! dh_agent_deductions(-NumberOfDeductions:nonneg) is det.

dh_agent_deductions(N):-
  number_of_deductions(N), !.
dh_agent_deductions(0).


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



% Messages

:- multifile(prolog:message//1).

prolog:message(entailment_fitness(Deductions,Lifetime,Fitness)) -->
  {dh_agent_graph(MyGraph)},
  ['[~w] ~f (~D entailments; ~D steps)'-[MyGraph,Fitness,Deductions,Lifetime]].

