:- module(dh_agent_forager, []).

/** <module> DataHives agent forager

Defines the forager bee agent for use in DataHives.

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06-2014/07
*/

:- dynamic(dh:agent_definition/2).
:- multifile(dh:agent_definition/2).
   dh:agent_definition(forager, [
     dh_random_walk-'Walk around randomly. This ensures locality',
     search_action(instance_of(foaf:'Person'), search_results)-'Search for instances of foaf:Person.',
     update_edge_count-'Communicate to the environment that a certain edge has been traversed.',
     aging(10)-'A forager walks around for a set number of steps and then terminates.'
   ]).

