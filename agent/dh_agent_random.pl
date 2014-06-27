:- module(dh_agent_random, []).

/** <module> DataHives random agent

Specification of a randomly traversing agent in DataHives.

@author Wouter Beek
@version 2014/06
*/

:- dynamic(user:agent_definition/2).
:- multifile(user:agent_definition/2).
   user:agent_definition(random, [
     dh_random_lod_walk,
     no_action,
     update_edge_count(1),
     no_evaluation
   ]).

