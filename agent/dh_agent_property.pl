:- module(
  dh_agent_property,
  [
    dh_agent_age/2, % ?Agent:atom
                    % ?Seconds:nonneg
    dh_agent_cycles/2, % +Agent:atom
                       % -NumberOfCycles:nonneg
    dh_agent_deductions/2, % +Agent:atom
                           % -NumberOfDeductions:nonneg
    dh_agent_steps/2 % +Agent:atom
                     % -NumberOfSteps:nonneg
  ]
).

/** <module> DataHives agent property

Access to the properties of individual agents.

@author Wouter Beek
@version 2014/08
*/

:- use_module(generics(vox_populi)).

:- use_module(dh_act(dh_entailment)).
:- use_module(dh_agent(dh_agent)).
:- use_module(dh_core(dh_cycle)).
:- use_module(dh_nav(dh_nav)).



%! dh_agent_age(+Agent:atom, -Age:nonneg) is det.

dh_agent_age(Agent, Age):-
  get_time(Now),
  ask_thread(Agent, dh_agent_creation, Creation),
  Age is Now - Creation.


%! dh_agent_cycles(+Agent:atom, -NumberOfCycles:nonneg) is det.
% Returns the number of cycles for the given agent thread.

dh_agent_cycles(Agent, Cycles):-
  ask_thread(Agent, dh_agent_cycles, Cycles).


%! dh_agent_deductions(+Agent:atom, -Deductions:nonneg) is det.

dh_agent_deductions(Agent, Deductions):-
  ask_thread(Agent, dh_agent_deductions, Deductions).


%! dh_agent_steps(+Agent:atom, -NumberOfSteps:nonneg) is det.
% Returns the number of steps for the given agent thread.

dh_agent_steps(Agent, Steps):-
  ask_thread(Agent, dh_agent_steps, Steps).


