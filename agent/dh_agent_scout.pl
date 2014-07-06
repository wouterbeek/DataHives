:- module(
  dh_agent_scout,
  [
    scout_communicate/1 % +DirectedTriple:compound
  ]
).

/** <module> DataHives agent scout

Defines the scout bee agent for use in DataHives.

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06-2014/07
*/

:- use_module(library(setting)).

:- use_module(dh_act(dh_edge_count)).
:- use_module(dh_act(dh_search)).
:- use_module(dh_agent(dh_agent)).
:- use_module(dh_agent(dh_agent_forager)).
:- use_module(dh_core(dh_generics)).

:- setting(
  number_of_foragers,
  nonneg,
  10,
  'The number of foragers that is spawned by a scout.'
).

:- dynamic(dh:agent_definition/2).
:- multifile(dh:agent_definition/2).
   dh:agent_definition(scout, [
     dh_random_lod_jump,
     search_action(instance_of(foaf:'Person')),
     scout_communicate(1),
     scout_aging(10),
     create_agent(scout)
   ]).



%! scout_aging(+MaxAge:positive_integer) is det.

scout_aging(MaxAge):-
  number_of_cycles(Age),
  (
    Age == MaxAge
  ->
    thread_exit(done)
  ;
    true
  ).


%! scout_communicate(+InterestLevel:nonneg, +DirectedTriple:compound) is det.

scout_communicate(InterestLevel, DirectedTriple):-
  update_edge_count(1, DirectedTriple),
  spawn_foragers(InterestLevel, DirectedTriple).


%! spawn_foragers(+InterestLevel:nonneg, +DirectedTriple:compound) is det.

spawn_foragers(InterestLevel, DirectedTriple):-
  thread_flag(number_of_search_results, NumberOfSearchResults),
  (
    NumberOfSearchResults >= InterestLevel
  ->
    setting(number_of_foragers, NumberOfForagers),
    directed_triple(DirectedTriple, Triple),
    forall(
      between(1, NumberOfForagers, _),
      create_agent(scout, Triple)
    )
  ;
    true
  ).

