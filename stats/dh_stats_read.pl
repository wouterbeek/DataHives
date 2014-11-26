:- module(
  dh_stats_read,
  [
    dh_stats_by_dataset/2, % +Dataset:iri
                           % -AgentValues:pair(iri,list)
    dh_stats_by_agent_property/3 % +Agent:iri
                                 % +Property:iri
                                 % -Values:list
  ]
).

/** <module> DataHives Statistics: read

Predicates for reading off statistic results from various measurements
performed in DataHives.

@author Wouter Beek
@version 2014/09, 2014/11
*/

:- use_module(library(aggregate)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_read)).

:- rdf_register_prefix(dct, 'http://purl.org/dc/terms/').
:- rdf_register_prefix(qb, 'http://purl.org/linked-data/cube#').
:- rdf_register_prefix('sdmx-dimension', 'http://purl.org/linked-data/sdmx/2009/dimension#').



%! dh_stats_by_dataset(+Dataset:iri, -AgentValues:pair(iri,list)) is det.

dh_stats_by_dataset(Dataset, Agent-Values):-
  once((
    rdf(Dataset, dct:subject, Agent),
    rdf(Dataset, qb:structure, DataStructureDefinition),
    rdf(DataStructureDefinition, qb:component, MeasureComponent),
    rdf(MeasureComponent, qb:measure, Property),
    rdf(Property, rdfs:range, Datatype)
  )),
  dh_stats0(Dataset, Property, Datatype, Values).



%! dh_stats_by_agent_property(+Agent:iri, +Property:iri, -Values:list) is det.

dh_stats_by_agent_property(Agent, Property, Values):-
  % Find a dataset for the given agent and property.
  % @tbd How to deal with multiple graphs?
  once((
    rdf(Dataset, dct:subject, Agent),
    rdf(Dataset, qb:structure, DataStructureDefinition),
    rdf(DataStructureDefinition, qb:component, MeasureComponent),
    rdf(MeasureComponent, qb:measure, Property),
    rdf(Property, rdfs:range, Datatype)
  )),
  dh_stats0(Dataset, Property, Datatype, Values).



dh_stats0(Dataset, Property, Datatype, Values):-
  findall(
    Time-Value,
    (
      rdf(Observation, qb:dataSet, Dataset),
      % @tbd We currently assume the temporal dimension.
      rdf_typed_literal(
        Observation,
        'sdmx-dimension':timePeriod,
        Time,
        xsd:dateTime,
        _
      ),
      rdf_typed_literal(Observation, Property, Value, Datatype, _)
    ),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  pairs_values(Pairs2, Values).

