:- module(
  dh_agent_scout,
  [
    spawn_foragers/2 % +InterestLevel:nonneg
                     % +DirectedTriple:compound
  ]
).

/** <module> DataHives agent scout

Defines the scout bee agent for use in DataHives.

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06-2014/07
*/

:- use_module(library(settings)).

:- use_module(generics(flag_ext)).

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
     dh_random_jump-'Jump to random locations in the LOD cloud time and again.',
     search_action(instance_of(foaf:'Person'), search_results)-'Search for instances of foaf:Person.',
     spawn_foragers(1)-'Communicate to the pool of foragers whether something has been found or not.',
     aging(10),
     create_agent(scout)
   ]).



%! spawn_foragers(+InterestLevel:nonneg, +DirectedTriple:compound) is det.

spawn_foragers(InterestLevel, DirectedTriple):-
  number_of_overall_search_results(NumberOfSearchResults),
  (
    NumberOfSearchResults >= InterestLevel
  ->
    setting(number_of_foragers, NumberOfForagers),
    directed_triple(DirectedTriple, Triple),
    forall(
      between(1, NumberOfForagers, _),
      create_agent(forager, Triple)
    )
  ;
    true
  ).

