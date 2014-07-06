:- module(dh_agent_random, []).

/** <module> DataHives random agent

Specification of a randomly traversing agent in DataHives.

@author Wouter Beek
@version 2014/06
*/

:- dynamic(dh:agent_definition/2).
:- multifile(dh:agent_definition/2).
   dh:agent_definition(random, [
     dh_random_walk,
     no_action,
     update_edge_count(1),
     no_evaluation
   ]).

