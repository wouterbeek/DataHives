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

:- dynamic(dh:dh_agent_property/2).
:- multifile(dh:dh_agent_property/2).
:- dynamic(dh:dh_agent_property/3).
:- multifile(dh:dh_agent_property/3).

:- rdf_meta(dh:dh_agent_property(r,?)).
:- rdf_meta(dh:dh_agent_property(?,r,?)).

:- initialization(init_agent_properties).



% @tbd Allow thread and graph names to be dissimilar.
dh:dh_agent_property(Property, Graph):-
  rdf_global_id(dho:graph, Property),
  thread_self(Thread),
  rdf(Agent, dho:thread, literal(type(Thread,xsd:anyURI)), dh),
  rdf(Agent, dho:graph, literal(type(Graph,xsd:anyURI)), dh).


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
  rdf_datatype(Agent, dho:creation, DateTimeValue, xsd:dateTime, dh),
  timeOnTimeline(DateTimeValue, Creation),

  Age is Now - Creation.
% CPU time
dh:dh_agent_property(Agent, Property, CpuTime):-
  rdf_global_id(dho:cpuTime, Property),
  dh:dh_agent_property(Agent, dho:thread, Thread),
  thread_property(Thread, status(Status)),
  (   memberchk(Status, [exception(_),false])
  ->  CpuTime = 0
  ;   thread_statistics(Thread, cputime, CpuTime)
  ).
% Effectiveness
dh:dh_agent_property(Agent, Property, Effectiveness):-
  rdf_global_id(dho:effectiveness, Property),
  dh:dh_agent_property(Agent, dho:steps, Steps),
  dh:dh_agent_property(Agent, dho:age, Age),
  Effectiveness is Steps / Age.
% Graph
dh:dh_agent_property(Agent, Property, Value):-
  rdf_global_id(dho:graph, Property),
  rdf_datatype(Agent, Property, Value, xsd:anyURI, dh).
% Status
dh:dh_agent_property(Agent, Property, Status):-
  rdf_global_id(dho:status, Property),
  dh:dh_agent_property(Agent, dho:thread, Thread),
  thread_property(Thread, status(Status)).
% Thread
dh:dh_agent_property(Agent, Property, Value):-
  rdf_global_id(dho:thread, Property),
gtrace,
  rdf_datatype(Agent, Property, Value, xsd:anyURI, dh).



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

