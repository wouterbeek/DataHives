:- module(dh_agent_definition_ant, []).

/** <module> DataHives Agent Definition: Ant

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06-2014/07, 2014/09
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).

:- use_module(dh(agent/definition/dh_agent_definition)).

:- initialization((
  register_dh_agent_definition(
    ant,
    [
      dh_weighted_walk,
      deductive_action,
      update_edge_count,
      evaluate_entailment,
      dh_agent_create(dh:'Agent/Ant')
    ]
  )
)).

