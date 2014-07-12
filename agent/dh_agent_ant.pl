:- module(dh_agent_ant, []).

/** <module> DataHives: ant agents

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06-2014/07
*/

:- dynamic(dh:agent_definition/2).
:- multifile(dh:agent_definition/2).
   dh:agent_definition(ant, [
     dh_weighted_walk,
     deductive_action,
     update_edge_count,
     evaluate_entailment,
     create_agent(ant)
   ]).

