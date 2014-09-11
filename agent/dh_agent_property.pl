:- module(
  dh_agent_property,
  [
  ]
).

/** <module> DataHives agent property

Access to the properties of individual agents.

@author Wouter Beek
@version 2014/08-2014/09
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(plXsd(xsd_rdf)).

:- use_module(plRdf(rdfs_build2)).

:- use_module(dh_act(dh_entailment)).
:- use_module(dh_agent(dh_agent)).
:- use_module(dh_agent(dh_agent_create)).
:- use_module(dh_core(dh_cycle)).
:- use_module(dh_core(dh_generics)). % RDF prefix
:- use_module(dh_core(dh_messages)).
:- use_module(dh_core(dh_population)).
:- use_module(dh_nav(dh_nav)).

:- dynamic(dh:dh_agent_property/2).
:- multifile(dh:dh_agent_property/2).
:- dynamic(dh:dh_agent_property/3).
:- multifile(dh:dh_agent_property/3).

:- rdf_meta(dh:dh_agent_property(r,?)).
:- rdf_meta(dh:dh_agent_property(?,r,?)).



% @tbd Allow thread and graph names to be dissimilar.
dh:dh_agent_property(dho:graph, Graph):-
  thread_self(Thread),
  thread_property(Thread, alias(Graph)).


%! dh:dh_agent_property(+Agent:iri, +Property:iri, +Value) is semidet.
%! dh:dh_agent_property(+Agent:iri, +Property:iri, -Value) is det.
%! dh:dh_agent_property(+Agent:iri, -Property:iri, -Value) is multi.
%! dh:dh_agent_property(-Agent:iri, -Property:iri, -Value) is nondet.

% Age
dh:dh_agent_property(Agent, dho:age, Age):-
  dh_agent(Agent),
  get_time(Now),
  dh_agent_ask(Agent, dh:dh_agent_property(dho:creation), Creation),
  Age is Now - Creation.
% CPU time
dh:dh_agent_property(Agent, dho:cpuTime, CpuTime):-
  dh:dh_agent_property(Agent, dho:thread, Thread),
  thread_property(Thread, status(Status)),
  (   memberchk(Status, [exception(_),false])
  ->  CpuTime = 0
  ;   thread_statistics(Thread, cputime, CpuTime)
  ).
% Effectiveness
dh:dh_agent_property(Agent, dho:effectiveness, Effectiveness):-
  dh:dh_agent_property(Agent, dho:steps, Steps),
  dh:dh_agent_property(Agent, dho:age, Age),
  Effectiveness is Steps / Age.
% Status
dh:dh_agent_property(Agent, dho:status, Status):-
  dh:dh_agent_property(Agent, dho:thread, Thread),
  thread_property(Thread, status(Status)).
% Asserted in RDF: Graph, Thread.
dh:dh_agent_property(Agent, Property, Value):-
  rdf(Agent, Property, Value, dh).


init_agent_properties:-
  xsd_assert_schema,
  rdfs_assert_property(
    dho:age,
    dho:agentProperty,
    dho:'Agent',
    xsd:duration,
    age,
    'The duration of the agent\'s lifespan.',
    dh
  ),
  rdfs_assert_property(
    dho:cpuTime,
    dho:agentProperty,
    dho:'Agent',
    xsd:float,
    'CPU time',
    'The CPU time that has been spend by an agent process \c
     since it was created.',
    dh
  ),
  rdfs_assert_property(
    dho:effectiveness,
    dho:agentProperty,
    dho:'Agent',
    xsd:float,
    effectiveness,
    'A non-basic agent property intended for testing purposes.',
    dh
  ),
  rdfs_assert_property(
    dho:graph,
    dho:agentProperty,
    dho:'Agent',
    xsd:anyURI,
    graph,
    'The RDF graph that is used to store the beliefs of an agent.',
    dh
  ),
  rdfs_assert_property(
    dho:status,
    dho:agentProperty,
    dho:'Agent',
    xsd:string,
    status,
    'The status of an agent process. \c
     This is either `true` (completed), `false` (Prolog fail), \c
     or a compound term describing a specific exception.',
    dh
  ),
  rdfs_assert_property(
    dho:thread,
    dho:agentProperty,
    dho:'Agent',
    xsd:string,
    thread,
    'The alias of the thread that is used to implement an agent.',
    dh
  ).

