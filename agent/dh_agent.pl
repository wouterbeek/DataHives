:- module(
  agent,
  [
    dh_list_agents/0
  ]
).

/** <module> DataHives agent

Generic support for agent definitions.

@author Wouter Beek
@version 2014/07
*/

dh_list_agents:-
  forall(
    user:agent_definition(Name, Definition),
    print_agent_definition(Name, Definition)
  ).

print_agent_definition(Name, Definition):-
  format('AGENT DEFINITION: ~a\n', [Name]),
  print_behaviors(
    ['NAVIGATE','ACT','COMMUNICATE','EVALUATE','REPRODUCE'],
    Definition
  ),
  format('\n').

print_behaviors(_, []).
print_behaviors([Prefix|T1], [Behavior|T2]):-
  format('  * ~a: ~17+~w\n', [Prefix,Behavior]),
  print_behaviors(T1, T2).

