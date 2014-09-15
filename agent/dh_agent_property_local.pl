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
@version 2014/09
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(plXsd(xsd_rdf)).

:- use_module(plRdf(rdfs_build2)).

:- use_module(dh_agent(dh_agent)).
:- use_module(dh_agent(dh_agent_property_global)).
:- use_module(dh_core(dh_messages)).

:- rdf_meta(dh_agent_property_local(r)).
:- rdf_meta(dh_agent_property_local(r,?)).
:- rdf_meta(dh_agent_property_local(?,r,?)).

:- initialization(init).



%! age(+Age:nonneg) is semidet.
%! age(-Age:nonneg) is det.

age(Age):-
  dh_agent_property_global(creation, Creation),
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
  xsd_assert_schema,
  rdfs_assert_property(
    dho:agentPropertyLocal,
    dho:agentProperty,
    dho:'Agent',
    rdfs:'Literal',
    'local agent property',
    'An agent property that is stored in a specific agent thread.',
    dho
  ),
  rdfs_assert_property(
    dho:age,
    dho:agentPropertyLocal,
    dho:'Agent',
    xsd:duration,
    age,
    'The duration of the agent\'s lifespan.',
    dho
  ),
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
