:- module(
  dh_agent_property,
  [
    dh_agent_property_name/1, % ?Name:atom
    dh_agent_property_name/2 % ?Name:atom
                             % ?Type:oneof([numeric])
  ]
).

/** <module> DataHives agent property

Access to the properties of individual agents.

@author Wouter Beek
@version 2014/08-2014/09
*/

:- use_module(library(semweb/rdfs)).

:- use_module(dh_act(dh_entailment)).
:- use_module(dh_agent(dh_agent)).
:- use_module(dh_agent(dh_agent_create)).
:- use_module(dh_core(dh_cycle)).
:- use_module(dh_core(dh_generics)). % RDF prefix
:- use_module(dh_core(dh_messages)).
:- use_module(dh_core(dh_population)).
:- use_module(dh_nav(dh_nav)).



% @tbd Allow thread and graph names to be dissimilar.
dh:dh_agent_property(graph, Graph):-
  thread_self(Thread),
  thread_property(Thread, alias(Graph)).


%! dh_agent_property(+Agent:url, +Name:atom, +Value) is semidet.
%! dh_agent_property(+Agent:url, +Name:atom, -Value) is det.
%! dh_agent_property(+Agent:url, -Name:atom, -Value) is multi.
%! dh_agent_property(-Agent:url, -Name:atom, -Value) is nondet.

% Age
dh:dh_agent_property(Agent, age, Age):-
  dh_agent(Agent),
  get_time(Now),
  dh_agent_ask(Agent, dh:dh_agent_property(creation), Creation),
  Age is Now - Creation.
% Class label
dh:dh_agent_property(Agent, classLabel, Label):-
  dh_agent(Agent),
  once((
    rdfs_individual_of(Agent, AgentDefinition),
    rdfs_subclass_of(AgentDefinition, dh:'Agent')
  )),
  rdfs_label(AgentDefinition, Label).
% CPU time
dh:dh_agent_property(Agent, cpuTime, CpuTime):-
  dh:dh_agent_property(Agent, thread, Thread),
  thread_property(Thread, status(Status)),
  (   memberchk(Status, [exception(_),false])
  ->  CpuTime = 0
  ;   thread_statistics(Thread, cputime, CpuTime)
  ).
% Effectiveness
dh:dh_agent_property(Agent, effectiveness, Effectiveness):-
  dh:dh_agent_property(Agent, steps, Steps),
  dh:dh_agent_property(Agent, age, Age),
  Effectiveness is Steps / Age.
% Graph
dh:dh_agent_property(Agent, graph, Graph):-
  rdfs_label(Agent, Graph).
% Label
dh:dh_agent_property(Agent, label, Label):-
  dh_agent(Agent),
  rdfs_label(Agent, Label).
% Status
dh:dh_agent_property(Agent, status, Status):-
  dh:dh_agent_property(Agent, thread, Thread),
  thread_property(Thread, status(Status)).
% Thread
dh:dh_agent_property(Agent, thread, Thread):-
  dh_agent(Agent),
  rdfs_label(Agent, Thread).



%! dh_agent_property_name(+Name:atom) is semidet.
%! dh_agent_property_name(-Name:atom) is multi.

dh_agent_property_name(Name):-
  dh_agent_property_name(Name, _).


%! dh_agent_property_name(+Name:atom, +Type:atom) is semidet.
%! dh_agent_property_name(+Name:atom, -Type:atom) is multi.

dh_agent_property_name(Name, Type):-
  dh:dh_agent_property_name0(Name, Type0),
  subclass(Type0, Type).

subclass(X, X):- !.
subclass(X, Z):-
  subclass0(X, Y),
  subclass(Y, Z).

subclass0(float,            numeric).
subclass0(integer,          numeric).
subclass0(nonneg,           integer).
subclass0(negative_integer, integer).
subclass0(positive_integer, integer).

dh:dh_agent_property_name0(age,           nonneg).
dh:dh_agent_property_name0(classLabel,    atom).
dh:dh_agent_property_name0(cpuTime,       float).
dh:dh_agent_property_name0(effectiveness, float).
dh:dh_agent_property_name0(graph,         atom).
dh:dh_agent_property_name0(label,         atom).
dh:dh_agent_property_name0(status,        compound).
dh:dh_agent_property_name0(thread,        atom).

