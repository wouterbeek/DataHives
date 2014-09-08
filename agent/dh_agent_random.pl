:- module(dh_agent_random, []).

/** <module> DataHives random agent

Specification of a randomly traversing agent in DataHives.

@author Wouter Beek
@version 2014/06, 2014/09
*/

:- use_module(library(http/http_dispatch)).

:- use_module(dh_agent(dh_agent_definition)).

:- initialization((
  http_link_to_id(dh_agent_definition, path_postfix(random), AgentDefinition),
  dh_agent_definition_db(
    AgentDefinition,
    [
      dh_random_walk,
      no_action,
      update_edge_count,
      no_evaluation
    ]
  )
)).

