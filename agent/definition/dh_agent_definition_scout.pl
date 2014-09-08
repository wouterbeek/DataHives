:- module(dh_agent_definition_scout, []).

/** <module> DataHives Agent Definition: Scout

Defines the scout bee agent for use in DataHives.

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06-2014/07, 2014/09
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).

:- use_module(plRdf(rdfs_label_ext)).

:- use_module(dh_act(dh_search)).
:- use_module(dh_agent_definition(dh_agent_definition)).
:- use_module(dh_agent_definition(dh_agent_definition_forager)).
:- use_module(dh_com(dh_edge_weight)).
:- use_module(dh_core(dh_generics)).

:- initialization((
  http_absolute_uri(dh_agent_definition(scout), AgentDefinition),
  rdfs_assert_label(AgentDefinition, scout, dh),
  dh_agent_definition_db(
    AgentDefinition,
    [
      dh_random_jump-"Jump to random locations in the LOD cloud time and again.",
      search_action(instance_of(foaf:'Person'), search_results)-"Search for instances of foaf:Person.",
      spawn_foragers(1, 10)-"Communicate to the pool of foragers whether something has been found or not.",
      no_evaluation,
      dh_agent_create(AgentDefinition)
    ]
  )
)).

