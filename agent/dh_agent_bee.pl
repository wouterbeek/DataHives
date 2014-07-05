:- module(
  dh_agent_bee,
  [
% Forager
    evaluate_forager/0,
    send_forager/0,
% Scout
    scout_action/1, % +DirectedTriple:compound
    evaluate_scout/0
  ]
).

/** <module> DataHives: bee agents

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06-2014/07
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(dh_act(dh_entailment)).
:- use_module(dh_agent(dh_agent)).
:- use_module(dh_com(dh_edge_weight)).
:- use_module(dh_core(dh_cycle)).
:- use_module(dh_core(dh_generic)).
:- use_module(dh_nav(dh_random_lod_jump)).
:- use_module(dh_nav(dh_random_lod_walk)).
:- use_module(dh_nav(dh_walk)).

:- dynamic(dh:agent_definition/2).
:- multifile(dh:agent_definition/2).
   dh:agent_definition(forager, [
     dh_random_lod_walk,
     deductive_action,
     update_edge_count(1),
     evaluate_forager
   ]).
   dh:agent_definition(scout, [
     dh_random_lod_jump,
     scout_action,
     update_edge_count(1),
     evaluate_scout,
     rebirth(scout)
   ]).



% FORAGER %

evaluate_forager:-
  number_of_cycles(Lifetime),
  (
    Lifetime > 10
  ->
    thread_exit(done)
  ;
    true
  ).


send_forager:-
  backtrack(DirTriple),
  directed_triple(DirTriple, Triple),
  send_forager(Triple).


send_forager(InitialTriple):-
  create_agents(5, forager, InitialTriple).



% SCOUT %

scout_action(dir(From,Dir,Link,To)):-
  deductive_action(dir(From,Dir,Link,To)),
  findall(
    Result,
    rdf(From, rdfs:comment, Result),
    Results
  ),
  length(Results,N),
  forall(
    between(1, N, _),
	  send_forager(From)
  ),
  increment_deductions(N).


evaluate_scout:-
  deductions(Deductions),
  number_of_cycles(Lifetime),
  Fitness is Deductions / Lifetime,
  (
    Fitness < 0.5
  ->
    thread_exit(done)
  ;
    Fitness > 1
  ->
    send_forager
  ;
    true
  ),
  print_message(informational, fitness(Deductions,Lifetime,Fitness)).
