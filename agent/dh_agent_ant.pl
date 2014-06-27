:- module(dh_agent_ant, []).

/** <module> DataHives: ant agents

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06
*/

:- dynamic(user:agent_definition/2).
:- multifile(user:agent_definition/2).
   user:agent_definition(ant, [
     dh_weighted_lod_walk,
     deductive_action,
     update_edge_count(1),
     evaluate_entailment,
     dh_ant_test
   ]).

