:- module(dh_agent_definition_ant, []).

/** <module> DataHives Agent Definition: Ant

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06-2014/07, 2014/09
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).

:- use_module(plRdf(rdfs_label_ext)).

:- use_module(dh_agent_definition(dh_agent_definition)).

:- initialization((
  http_absolute_uri(dh_agent_definition(ant), AgentDefinition),
  rdfs_assert_label(AgentDefinition, ant, dh),
  dh_agent_definition_db(
    AgentDefinition,
    [
      dh_weighted_walk,
      deductive_action,
      update_edge_count,
      evaluate_entailment,
      dh_agent_create(AgentDefinition)
    ]
  )
)).

