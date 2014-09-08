:- module(dh_agent_ant, []).

/** <module> DataHives: ant agents

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06-2014/07, 2014/09
*/

:- use_module(library(http/http_dispatch)).

:- use_module(dh_agent(dh_agent_definition)).

:- initialization((
  http_link_to_id(dh_agent_definition, path_postfix(ant), AgentDefinition),
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

