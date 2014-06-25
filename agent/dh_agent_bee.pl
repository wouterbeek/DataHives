:- module(
  dh_agent_bee,
  [
% FORAGER
    evaluate_forager/0,
    send_forager/0,
% SCOUT
    scout_action/4, % +From:or([bnode,iri,literal])
                    % -Direction:oneof([backward,forward])
                    % -Link:iri
                    % -To:or([bnode,iri,literal])
    evaluate_scout/0
  ]
).

/** <module> DataHives: bee agents

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06
*/

:- use_module(dh_agent(dh_agent_entailment)).
:- use_module(dh_com(dh_edge_weight)).
:- use_module(dh_core(dh_agent)).
:- use_module(dh_core(dh_cycle)).
:- use_module(dh_core(dh_navigate)).
:- use_module(dh_nav(dh_weighted_lod_walk)).



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
  backtrack(From, _, _, _),
  create_agent(
    dh_weighted_lod_walk,
    deductive_action,
    update_edge_count(1),
    evaluate_forager,
    From,
    5
  ).



% SCOUT %

evaluate_scout:-
  deductions(Deductions),
  number_of_cycles(Lifetime),
  Fitness is Deductions / Lifetime,
  (
    Fitness < 0.5
  ->
    thread_exit(done)
  ;
    Fitness > 0.8
  ->
    send_forager
  ;
    true
  ),
  print_message(informational, fitness(Deductions,Lifetime,Fitness)).


scout_action(From, Dir, Link, To):-
  deductive_action(From, Dir, Link, To).
