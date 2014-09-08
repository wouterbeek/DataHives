:- module(dh_agent_definition_random, []).

/** <module> DataHives Agent Definition: Random

Specification of a randomly traversing agent in DataHives.

@author Wouter Beek
@version 2014/06, 2014/09
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).

:- use_module(plRdf(rdfs_label_ext)).

:- use_module(dh_agent_definition(dh_agent_definition)).

:- initialization((
  http_absolute_uri(dh_agent_definition(random), AgentDefinition),
  rdfs_assert_label(AgentDefinition, random, dl),
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

