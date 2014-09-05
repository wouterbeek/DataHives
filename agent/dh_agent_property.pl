:- module(
  dh_agent_property,
  [
    dh_agent_age/2, % +Agent
                    % -Age:float
    dh_agent_cycles/2, % +Agent
                       % -NumberOfCycles:nonneg
    dh_agent_deductions/2, % +Agent
                           % -NumberOfDeductions:nonneg
    dh_agent_property/3, % +Agent
                         % +Name:oneof([age,cycles,deductions,steps])
                         % -Value
    dh_agent_steps/2 % +Agent
                     % -NumberOfSteps:nonneg
  ]
).

/** <module> DataHives agent property

Access to the properties of individual agents.

@author Wouter Beek
@version 2014/08-2014/09
*/

:- use_module(generics(vox_populi)).

:- use_module(dh_act(dh_entailment)).
:- use_module(dh_agent(dh_agent)).
:- use_module(dh_core(dh_cycle)).
:- use_module(dh_nav(dh_nav)).



%! dh_agent_age(+Agent, -Age:float) is det.

dh_agent_age(Agent, Age):-
  get_time(Now),
  ask_thread(Agent, dh_agent_creation, Creation),
  Age is Now - Creation.


%! dh_agent_cycles(+Agent, -NumberOfCycles:nonneg) is det.
% Returns the number of cycles for the given agent thread.

dh_agent_cycles(Agent, Cycles):-
  ask_thread(Agent, dh_agent_cycles, Cycles).


%! dh_agent_deductions(+Agent, -Deductions:nonneg) is det.

dh_agent_deductions(Agent, Deductions):-
  ask_thread(Agent, dh_agent_deductions, Deductions).


%! dh_agent_property(
%!   +Agent,
%!   +Name:oneof([age,cycles,deductions,steps]),
%!   -Value
%! ) is det.

dh_agent_property(Agent, Name, Value):-
  atomic_list_concat([dh_agent,Name], '_', Pred),
  call(Pred, Agent, Value).


%! dh_agent_steps(+Agent, -NumberOfSteps:nonneg) is det.
% Returns the number of steps for the given agent thread.

dh_agent_steps(Agent, Steps):-
  ask_thread(Agent, dh_agent_steps, Steps).

