:- module(
  dh_agent_property_global,
  [
    cpuTime/2, % ?Agent:iri
               % -CpuTime:float
    creation/2, % ?Agent:iri
                % -Creation:float
    dh_agent_property_global/1, % ?Property:iri
    dh_agent_property_global/2, % ?Property:iri
                                % ?Value
    dh_agent_property_global/3, % ?Agent:iri
                                % ?Property:iri
                                % ?Value
    status/2 % ?Agent:iri
             % ?Status:compound
  ]
).

/** <module> DataHives agent property

Global agent properties,
i.e. agent properties that can be access from within the generic environemtn.

Local agent properties are defined in [dh_agent_property_local].
They can be accessed with dh_agent_property/3.

@author Wouter Beek
@version 2014/08-2014/09
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- rdf_meta(dh_agent_property_global(r)).
:- rdf_meta(dh_agent_property_global(r,?)).
:- rdf_meta(dh_agent_property_global(?,r,?)).

:- use_module(plDcg(dcg_generic)).

:- use_module(plXsd(xsd_rdf)).
:- use_module(plXsd_datetime(xsd_dateTime)).
:- use_module(plXsd_datetime(xsd_dateTime_support)).

:- use_module(plRdf(rdfs_build2)).
:- use_module(plRdf_term(rdf_datatype)).

:- use_module(dh_act(dh_entailment)).
:- use_module(dh_agent(dh_agent)).
:- use_module(dh_agent(dh_agent_create)).
:- use_module(dh_agent(dh_agent_property_local)).
:- use_module(dh_core(dh_cycle)).
:- use_module(dh_core(dh_generics)). % RDF prefix
:- use_module(dh_core(dh_messages)).
:- use_module(dh_core(dh_population)).
:- use_module(dh_nav(dh_nav)).

:- initialization(init).



% cpuTime(+Agent:iri, -CpuTime:float) is det.
% cpuTime(-Agent:iri, -CpuTime:float) is nondet.

cpuTime(Agent, CpuTime):-
  dh_agent(Agent),
  thread_property(Agent, status(Status)),
  (   memberchk(Status, [exception(_),false])
  ->  CpuTime = 0
  ;   thread_statistics(Agent, cputime, CpuTime)
  ).


%! creation(+Agent:iri, -Creation:float) is det.
%! creation(-Agent:iri, -Creation:float) is nondet.

creation(Agent, Creation):-
  rdf_datatype(Agent, dho:creation, Creation0, xsd:dateTime, dh),
  timeOnTimeline(Creation0, Creation).


%! dh_agent_property_global(+Property:iri) is semidet.
%! dh_agent_property_global(-Property:iri) is multi.
% Global agent properties.

dh_agent_property_global(Property):-
  rdfs_subproperty_of(Property, dho:agentPropertyGlobal),
  \+ ((
    rdfs_subproperty_of(Property0, Property),
    Property0 \== Property
  )).


%! dh_agent_property_global(+Property:iri, +Value) is semidet.
% Access global properties of the current agent.

dh_agent_property_global(Property, Value):-
  agent_self(Agent),
  dh_agent_property_global(Agent, Property, Value).


%! dh_agent_property_global(?Agent:iri, ?Property:iri, ?Value) is multi.
% Access global properties of any agent.

dh_agent_property_global(Agent, Property, Value):-
  dh_agent(Agent),
  dh_agent_property_global(Property),
  rdf_global_id(dho:Goal, Property),
  call(Goal, Agent, Value).


%! status(+Agent:iri, ?Status:compound) is det.
%! status(-Agent:iri, ?Status:compound) is nondet.

status(Agent, Status):-
  dh_agent(Agent),
  thread_property(Agent, status(Status)).



init:-
  xsd_assert_schema,
  rdfs_assert_property(
    dho:agentPropertyGlobal,
    dho:agentProperty,
    dho:'Agent',
    rdfs:'Literal',
    'global agent property',
    'An agent property that is stored in the global environment.',
    dho
  ),
  
  % CPU time
  rdfs_assert_property(
    dho:cpuTime,
    dho:agentPropertyGlobal,
    dho:'Agent',
    xsd:float,
    'CPU time',
    'The CPU time that has been spend by an agent process \c
     since it was created.',
    dho
  ),
  
  % Creation
  rdfs_assert_property(
    dho:creation,
    dho:agentPropertyGlobal,
    dho:'Agent',
    xsd:float,
    creation,
    'The date and time at which an agent was created.',
    dho
  ),
  
  % Status
  rdfs_assert_property(
    dho:status,
    dho:agentPropertyGlobal,
    dho:'Agent',
    xsd:string,
    status,
    'The status of an agent process. \c
     This is either `true` (completed), `false` (Prolog fail), \c
     or a compound term describing a specific exception.',
    dho
  ).

