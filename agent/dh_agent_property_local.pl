:- module(
  dh_agent_property_local,
  [
    age/1, % ?Age:float
    agent_self/1, % -Agent:iri
    dh_agent_property_local/1, % ?Property:iri
    dh_agent_property_local/2, % ?Property:iri
                               % ?Value
    dh_agent_property_local/3, % ?Agent:iri
                               % ?Property:iri
                               % ?Value
    graph_self/1 % -Graph:iri
  ]
).

/** <module> DataHives Agent: Reflection

Local agent properties,
i.e. agent properties that can only be access from within
a specific agent thread.

Global agent properties are defined in [dh_agent_property_global].
They can be accessed with dh_agent_property/2.

@author Wouter Beek
@version 2014/09, 2014/11
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).

:- use_module(plRdf(api/rdfs_build2)).
:- use_module(plRdf(owl/owl_build)).

:- use_module(dh(agent/dh_agent)).
:- use_module(dh(agent/dh_agent_property_global)).
:- use_module(dh(beh/act/dh_entailment)). % Deductions property.
:- use_module(dh(beh/nav/dh_nav)). % Steps property.
:- use_module(dh(core/dh_cycle)). % Cycle property.
:- use_module(dh(core/dh_messages)).

:- rdf_meta(dh_agent_property_local(r)).
:- rdf_meta(dh_agent_property_local(r,?)).
:- rdf_meta(dh_agent_property_local(?,r,?)).

:- rdf_register_prefix(
     'sdmx-dimension',
     'http://purl.org/linked-data/sdmx/2009/dimension#'
   ).

:- initialization(init).



%! age(+Age:nonneg) is semidet.
%! age(-Age:nonneg) is det.

age(Age):-
  % @tbd RDF prefix is not resolved!
  rdf_global_id(dho:creation, Property),
  dh_agent_property_global(Property, Creation),
  get_time(Now),
  Age is Now - Creation.


%! agent_self(-Agent:iri) is det.

agent_self(Agent):-
  thread_self(Agent).


%! dh_agent_property_local(?Property:iri, ?Value) is multi.

dh_agent_property_local(Property):-
  rdfs_subproperty_of(Property, dho:agentPropertyLocal),
  \+ ((
    rdfs_subproperty_of(Property0, Property),
    Property0 \== Property
  )).


%! dh_agent_property_local(?Property:iri, ?Value) is multi.
% Enumerates the local agent properties.

dh_agent_property_local(Property, Value):-
  dh_agent_property_local(Property),
  rdf_global_id(dho:Goal, Property),
  call(Goal, Value).


%! dh_agent_property_local(?Agent:iri, ?Property:iri, ?Value) is det.

dh_agent_property_local(Agent, Property, Value):-
  dh_agent(Agent),
  dh_agent_property_local(Property),
  dh_agent_ask(Agent, dh_agent_property_local(Property), Value).


%! graph_self(-Graph:iri) is det.

graph_self(Graph):-
  thread_self(Graph).



init:-
  rdfs_assert_property(
    dho:agentPropertyLocal,
    dho:agentProperty,
    dho:'Agent',
    rdfs:'Literal',
    'local agent property',
    'An agent property that is stored in a specific agent thread.',
    dho
  ),
  
  % Age
  rdfs_assert_property(
    dho:age,
    dho:agentPropertyLocal,
    dho:'Agent',
    xsd:duration,
    age,
    'The duration of the agent\'s lifespan.',
    dho
  ),
  owl_assert_resource_identity(dho:age, 'sdmx-dimension':age, dho),
  
  % Cycles
  rdfs_assert_property(
    dho:cycles,
    dho:agentPropertyLocal,
    dho:'Agent',
    xsd:nonNegativeInteger,
    cycles,
    'The number of cycles that have been run for an agent \c
     since it was created.',
    dho
  ).

