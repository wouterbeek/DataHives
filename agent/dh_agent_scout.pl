:- module(
  dh_agent_scout,
  [
    spawn_foragers/3 % +InterestLevel:nonneg
                     % +NumberOfForagers:nonneg
                     % +DirectedTriple:compound
  ]
).

/** <module> DataHives agent scout

Defines the scout bee agent for use in DataHives.

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06-2014/07
*/

:- use_module(dh_act(dh_search)).
:- use_module(dh_agent(dh_agent)).
:- use_module(dh_agent(dh_agent_forager)).
:- use_module(dh_com(dh_edge_weight)).
:- use_module(dh_core(dh_generics)).

:- dynamic(dh:agent_definition/2).
:- multifile(dh:agent_definition/2).
   dh:agent_definition(scout, [
     dh_random_jump-"Jump to random locations in the LOD cloud time and again.",
     search_action(instance_of(foaf:'Person'), search_results)-"Search for instances of foaf:Person.",
     spawn_foragers(1, 10)-"Communicate to the pool of foragers whether something has been found or not.",
     no_evaluation,
     create_agent(scout)
   ]).



%! spawn_foragers(
%!   +InterestLevel:nonneg,
%!   +NumberOfForagers:nonneg,
%!   +DirectedTriple:compound
%! ) is det.

spawn_foragers(InterestLevel, NumberOfForagers, DirectedTriple):-
  update_edge_count(DirectedTriple), %DEB
  number_of_overall_search_results(NumberOfSearchResults),
  (
    NumberOfSearchResults >= InterestLevel
  ->
    directed_triple(DirectedTriple, Triple),
    forall(
      between(1, NumberOfForagers, _),
      create_agent(forager, Triple)
    )
  ;
    true
  ).

