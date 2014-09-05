:- module(
  dh_agent_defs,
  [
    list_agent_defs/0
  ]
).

/** <module> DataHives agent documentation

@author Wouter Beek
@version 2014/07, 2014/09
*/



%! list_agent_defs is det.
% Lists all currently loaded agent definitions on current output.

list_agent_defs:-
  forall(
    dh:agent_definition(Name, Definition),
    print_agent_definition(Name, Definition)
  ).

print_agent_definition(Name, BehaviorDefinitions):-
  format('AGENT DEFINITION: ~a\n', [Name]),
  print_behavior_definitions(
    ['NAVIGATE','ACT','COMMUNICATE','EVALUATE','EXIT'],
    BehaviorDefinitions
  ),
  format('\n').

print_behavior_definition(Predicate-Documentation):- !,
  print_behavior_predicate(Predicate),
  format('~18+~a\n', [Documentation]).
print_behavior_definition(Predicate):-
  print_behavior_predicate(Predicate).

print_behavior_definitions(_, []).
print_behavior_definitions([Prefix|T1], [BehaviorDefinition|T2]):-
  format('  * ~a: ~18+', [Prefix]),
  print_behavior_definition(BehaviorDefinition),
  print_behavior_definitions(T1, T2).

print_behavior_predicate(Predicate):-
  format('~w\n', [Predicate]).

