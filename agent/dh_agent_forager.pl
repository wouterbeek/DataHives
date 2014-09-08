:- module(dh_agent_forager, []).

/** <module> DataHives agent forager

Defines the forager bee agent for use in DataHives.

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06-2014/07, 2014/09
*/

:- use_module(library(http/http_dispatch)).

:- use_module(dh_agent(dh_agent_definition)).

:- initialization((
  http_link_to_id(
    dh_agent_definition,
    path_postfix(forager),
    AgentDefinition
  ),
  dh_agent_definition_db(
    AgentDefinition,
    [
      dh_random_walk-"Walk around randomly. This ensures locality",
      search_action(instance_of(foaf:'Person'), search_results)-"Search for instances of foaf:Person.",
      update_edge_count-"Communicate to the environment that a certain edge has been traversed.",
      aging(10)-"A forager walks around for a set number of steps and then terminates."
    ]
  )
)).

