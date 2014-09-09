:- module(
  dh_agent_property,
  [
    dh_agent_graph/1, % -Graph:url
    dh_agent_property/1, % ?Name:atom
    dh_agent_property/3 % ?Agent:url
                        % ?Name:atom
                        % ?Value
  ]
).

/** <module> DataHives agent property

Access to the properties of individual agents.

@author Wouter Beek
@version 2014/08-2014/09
*/

:- use_module(library(semweb/rdfs)).

:- use_module(generics(vox_populi)).

:- use_module(dh_act(dh_entailment)).
:- use_module(dh_agent(dh_agent)).
:- use_module(dh_agent(dh_agent_create)).
:- use_module(dh_core(dh_cycle)).
:- use_module(dh_core(dh_generics)). % RDF prefix
:- use_module(dh_core(dh_population)).
:- use_module(dh_nav(dh_nav)).



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


%! dh_agent_property(+Name:atom) is semidet.
%! dh_agent_property(-Name:atom) is multi.

dh_agent_property(age).
dh_agent_property(classLabel).
dh_agent_property(cpuTime).
dh_agent_property(cycles).
dh_agent_property(deductions).
dh_agent_property(effectiveness).
dh_agent_property(label).
dh_agent_property(status).
dh_agent_property(steps).
dh_agent_property(thread).


%! dh_agent_property(+Agent:url, +Name:atom, +Value) is semidet.
%! dh_agent_property(+Agent:url, +Name:atom, -Value) is det.
%! dh_agent_property(+Agent:url, -Name:atom, -Value) is multi.
%! dh_agent_property(-Agent:url, -Name:atom, -Value) is nondet.

% Age
dh_agent_property(Agent, age, Age):-
  dh_agent(Agent),
  get_time(Now),
  ask_thread(Agent, dh_agent_creation, Creation),
  Age is Now - Creation.
% Class label
dh_agent_property(Agent, classLabel, Label):-
  dh_agent(Agent),
  once((
    rdfs_individual_of(Agent, AgentDefinition),
    rdfs_subclass_of(AgentDefinition, dh:'Agent')
  )),
  rdfs_label(AgentDefinition, Label).
% CPU time
dh_agent_property(Agent, cpuTime, CpuTime):-
  dh_agent_property(Agent, thread, Thread),
  thread_property(Thread, status(Status)),
  (   memberchk(Status, [exception(_),false])
  ->  CpuTime = 0
  ;   thread_statistics(Thread, cputime, CpuTime)
  ).
% Cycles
dh_agent_property(Agent, cycles, Cycles):-
  dh_agent(Agent),
  ask_thread(Agent, dh_agent_cycles, Cycles).
% Deductions
dh_agent_property(Agent, deductions, Deductions):-
  dh_agent(Agent),
  ask_thread(Agent, dh_agent_deductions, Deductions).
% Effectiveness
dh_agent_property(Agent, effectiveness, Effectiveness):-
  dh_agent_property(Agent, steps, Steps),
  dh_agent_property(Agent, age, Age),
  Effectiveness is Steps / Age.
% Label
dh_agent_property(Agent, label, Label):-
  dh_agent(Agent),
  rdfs_label(Agent, Label).
% Status
dh_agent_property(Agent, status, Status):-
  dh_agent_property(Agent, thread, Thread),
  thread_property(Thread, status(Status)).
% Steps
dh_agent_property(Agent, steps, Steps):-
  dh_agent(Agent),
  ask_thread(Agent, dh_agent_steps, Steps).
% Thread
dh_agent_property(Agent, thread, Thread):-
  dh_agent(Agent),
  rdfs_label(Agent, Thread).

