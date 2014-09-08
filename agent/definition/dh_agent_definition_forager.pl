:- module(dh_agent_definition_forager, []).

/** <module> DataHives Agent Definition: forager

Defines the forager bee agent for use in DataHives.

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06-2014/07, 2014/09
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).

:- use_module(plRdf(rdfs_label_ext)).

:- use_module(dh_agent_definition(dh_agent_definition)).

:- initialization((
  http_absolute_uri(dh_agent_definition(forager), AgentDefinition),
  rdfs_assert_label(AgentDefinition, forager, dh),
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

