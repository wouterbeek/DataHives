:- module(
  dh_agent_property,
  [
    agent_self/1, % -Agent:iri
    cycles/1, % -Cycles:nonneg
    graph_self/1 % -Graph:iri
  ]
).

/** <module> DataHives agent property

Access to the properties of individual agents.

@author Wouter Beek
@version 2014/08-2014/09
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(plDcg(dcg_generic)).

:- use_module(plXsd(xsd_rdf)).
:- use_module(plXsd_datetime(xsd_dateTime)).
:- use_module(plXsd_datetime(xsd_dateTime_support)).

:- use_module(plRdf(rdfs_build2)).
:- use_module(plRdf_term(rdf_datatype)).

:- use_module(dh_act(dh_entailment)).
:- use_module(dh_agent(dh_agent)).
:- use_module(dh_agent(dh_agent_create)).
:- use_module(dh_core(dh_cycle)).
:- use_module(dh_core(dh_generics)). % RDF prefix
:- use_module(dh_core(dh_messages)).
:- use_module(dh_core(dh_population)).
:- use_module(dh_nav(dh_nav)).

:- initialization(init_agent_properties).



%! agent_self(-Agent:iri) is det.

agent_self(Agent):-
  thread_self(Agent).


% cycles(+Cycles:nonneg) is semidet.
% cycles(-Cycles:nonneg) is det.

cycles(Cycles):-
  dh_cycle:number_of_cycles(Cycles), !.
cycles(0).


%! graph_self(-Graph:iri) is det.

graph_self(Graph):-
  thread_self(Graph).


%! dh:dh_agent_property(+Agent:iri, +Property:iri, +Value) is semidet.
%! dh:dh_agent_property(+Agent:iri, +Property:iri, -Value) is det.
%! dh:dh_agent_property(+Agent:iri, -Property:iri, -Value) is multi.
%! dh:dh_agent_property(-Agent:iri, -Property:iri, -Value) is nondet.

% Age
dh:dh_agent_property(Agent, Property, Age):-
  rdf_global_id(dho:age, Property),
  dh_agent(Agent),
  get_time(Now),

  % The creation time must be parsed.
  dh:dh_agent_property(Agent, dho:creation, DateTimeValue),
  timeOnTimeline(DateTimeValue, Creation),

  Age is Now - Creation.
% CPU time
dh:dh_agent_property(Agent, Property, CpuTime):-
  rdf_global_id(dho:cpuTime, Property),
  thread_property(Agent, status(Status)),
  (   memberchk(Status, [exception(_),false])
  ->  CpuTime = 0
  ;   thread_statistics(Agent, cputime, CpuTime)
  ).
% Creation
dh:dh_agent_property(Agent, Property, Creation):-
  rdf_global_id(dho:creation, Property),
  dh_agent(Agent),
  rdf_datatype(Agent, Property, Creation, xsd:dateTime, dh).
% Effectiveness
dh:dh_agent_property(Agent, Property, Effectiveness):-
  rdf_global_id(dho:effectiveness, Property),
  dh:dh_agent_property(Agent, dho:steps, Steps),
  dh:dh_agent_property(Agent, dho:age, Age),
  Effectiveness is Steps / Age.
% Status
dh:dh_agent_property(Agent, Property, Status):-
  rdf_global_id(dho:status, Property),
  thread_property(Agent, status(Status)).



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
    dho:creation,
    dho:agentProperty,
    dho:'Agent',
    xsd:float,
    creation,
    'The date and time at which an agent was created.',
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
    dho:status,
    dho:agentProperty,
    dho:'Agent',
    xsd:string,
    status,
    'The status of an agent process. \c
     This is either `true` (completed), `false` (Prolog fail), \c
     or a compound term describing a specific exception.',
    dh
  ).

