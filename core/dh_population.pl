:- module(
  dh_population,
  [
    dh_population_delete/0,
    dh_population_property/2 % ?Property:iri
                             % ?Value
  ]
).

/** <module> DataHives: population

Population management and statistics for DataHives.

At the moment, at most one population can be formed.
The population is the collection of all agent threads that are active.

@author Wouter Beek
@version 2014/06-2014/09
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(generics(thread_ext)).
:- use_module(generics(vox_populi)).

:- use_module(plRdf(rdfs_build2)).

:- use_module(dh_agent(dh_agent)).
:- use_module(dh_agent(dh_agent_create)).
:- use_module(dh_agent(dh_agent_property)).
:- use_module(dh_com(dh_edge_weight)).

:- rdf_meta(dh_population_property(r,?)).

:- initialization(init).



%! dh_population_delete is det.

dh_population_delete:-
  dh_population_property(members, Agents),
  maplist(dh_agent_delete, Agents).


%! dh_population_property(+Property:iri, +Value) is semidet.
%! dh_population_property(+Property:iri, -Value) is det.
%! dh_population_property(-Property:iri, -Value) is nondet.

dh_population_property(dho:members, Value):-
  aggregate_all(
    set(Agent),
    dh_agent(Agent),
    Value
  ).
dh_population_property(dho:size, Value):-
  dh_population_property(dho:members, Agents),
  length(Agents, Value).
dh_population_property(Property, Value):-
  % Retrieve the agents belonging to the current population.
  dh_population_property(dho:members, Agents),
  
  % Retrieve the values for the given property: one for each agent.
  maplist(
    \Agent^Value^dh_agent_property(Agent, Property, Value),
    Agents,
    Values
  ),
  
  % Post-processing on values, based on the property's datatype.
  (   rdf(Property, rdfs:range, Datatype0, dho),
      rdfs_subclass_of(Datatype0, dho:numeric)
  ->  sum_list(Values, Value)
  ;   Value = Values
  ).

init:-
  rdfs_assert_property(
    dho:members,
    dho:populationProperty,
    dho:'Population',
    rdf:'Bag',
    members,
    'The members (agents) of a population.',
    dho
  ),
  rdfs_assert_property(
    dho:size,
    dho:populationProperty,
    dho:'Population',
    xsd:nonNegativeInteger,
    'population size',
    'The number of agents that belong to a population.',
    dho
  ).

