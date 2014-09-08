:- module(
  dh_agent_property,
  [
    dh_agent_age/2, % +Agent:url
                    % -Age:float
    dh_agent_cycles/2, % +Agent:url
                       % -NumberOfCycles:nonneg
    dh_agent_deductions/2, % +Agent:url
                           % -NumberOfDeductions:nonneg
    dh_agent_graph/1, % -Graph:url
    dh_agent_property/3, % +Agent:url
                         % ?Name:oneof([age,cycles,deductions,steps])
                         % ?Value
    dh_agent_steps/2 % +Agent:url
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
:- use_module(dh_agent(dh_agent_create)).
:- use_module(dh_core(dh_cycle)).
:- use_module(dh_core(dh_population)).
:- use_module(dh_nav(dh_nav)).



%! dh_agent_age(+Agent:url, -Age:float) is det.

dh_agent_age(Agent, Age):-
  get_time(Now),
  ask_thread(Agent, dh_agent_creation, Creation),
  Age is Now - Creation.


%! dh_agent_cycles(+Agent:url, -NumberOfCycles:nonneg) is det.
% Returns the number of cycles for the given agent thread.

dh_agent_cycles(Agent, Cycles):-
  ask_thread(Agent, dh_agent_cycles, Cycles).


%! dh_agent_deductions(+Agent:url, -Deductions:nonneg) is det.

dh_agent_deductions(Agent, Deductions):-
  ask_thread(Agent, dh_agent_deductions, Deductions).


%! dh_agent_graph(-Graph:url) is semidet.
% Returns the name of this agent's RDF graph.
%
% With the phrase "this agent" we mean the agent that is
% implemented by the thread from within which this predicate is called.
%
% Silently fails if the current thread is not an agent.

dh_agent_graph(Graph):-
  thread_self(Me),

  % Make sure the current thread denotes an agent.
  dh_agent_thread(Me),

  % We use the assumption that the alias of an agent
  % is also the name of the RDF graph of an agent.
  thread_property(Me, alias(Graph)).


%! dh_agent_property(
%!   +Agent:url,
%!   +Name:oneof([age,cycles,deductions,steps]),
%!   -Value
%! ) is det.

dh_agent_property(Agent, Name, Value):-
  atomic_list_concat([dh_agent,Name], '_', Pred),
  call(Pred, Agent, Value).


%! dh_agent_steps(+Agent:url, -NumberOfSteps:nonneg) is det.
% Returns the number of steps for the given agent thread.

dh_agent_steps(Agent, Steps):-
  ask_thread(Agent, dh_agent_steps, Steps).

