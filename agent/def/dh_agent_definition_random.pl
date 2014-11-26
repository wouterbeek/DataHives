:- module(dh_agent_definition_random, []).

/** <module> DataHives Agent Definition: Random

Specification of a randomly traversing agent in DataHives.

@author Wouter Beek
@version 2014/06, 2014/09
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).

:- use_module(dh(agent/def/dh_agent_definition)).

:- initialization((
  register_dh_agent_definition(
    random,
    [
      dh_random_walk,
      no_action,
      update_edge_count,
      no_evaluation
    ]
  )
)).

