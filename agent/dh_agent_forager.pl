:- module(
  dh_agent_forager,
  [
    evaluate_forager/0,
    send_forager/0,
  ]
).

/** <module> DataHives agent forager

Defines the forager bee agent for use in DataHives.

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06-2014/07
*/

:- use_module(generics(flag_ext)).

:- use_module(dh_act(dh_search)).
:- use_module(dh_core(dh_cycle)).

:- dynamic(dh:agent_definition/2).
:- multifile(dh:agent_definition/2).
   dh:agent_definition(forager, [
     dh_random_lod_walk,
     search_for_class_results(instance_of(foaf:'Person')),
     update_edge_count(1),
     no_evaluation,
     report_results
   ]).



terminate_forager:-
  number_of_cycles(Lifetime),
  (
    Lifetime > 10
  ->
    thread_exit(done)
  ;
    true
  ).

