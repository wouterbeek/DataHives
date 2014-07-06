:- module(dh_agent_ant, []).

/** <module> DataHives: ant agents

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06
*/

:- dynamic(dh:agent_definition/2).
:- multifile(dh:agent_definition/2).
   dh:agent_definition(ant, [
     dh_weighted_lod_walk,
     deductive_action,
     update_edge_count(1),
     evaluate_entailment,
     rebirth(ant)
   ]).

