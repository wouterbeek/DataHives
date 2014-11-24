:- module(
  dh_stats_write,
  [
    dh_stats_loop/5 % +Agents:list(iri)
                    % +Property:iri
                    % +Interval:positive_integer
                    % +Graph:atom
                    % -Dataset:iri
  ]
).

/** <module> DataHives Statistics: Write

Writes statistics data in DataHives.

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(lambda_meta)).
:- use_module(generics(thread_ext)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(vocabulary/datacube)).

:- use_module(dh(agent/dh_agent_property)).

:- rdf_meta(dh_stats_loop(+,r,+,+,-)).

:- rdf_register_prefix(dct, 'http://purl.org/dc/terms/').
:- rdf_register_prefix(qb, 'http://purl.org/linked-data/cube#').
:- rdf_register_prefix('sdmx-dimension', 'http://purl.org/linked-data/sdmx/2009/dimension#').



%! dh_stats_loop(
%!   +Agents:list(iri),
%!   +Property:iri,
%!   +Interval:positive_integer,
%!   +Graph:atom,
%!   -Dataset:iri
%! ) is det.
% Runs an intermittent loop of performing measurements every N seconds.

% Multiple agents.
dh_stats_loop(Agents, Property, Interval, Graph, Datasets):-
  is_list(Agents), !,
  maplist(
    \Agent^Dataset^dh_stats_loop(Agent, Property, Interval, Graph, Dataset),
    Agents,
    Datasets
  ).
% A single agent.
dh_stats_loop(Agent, Property, Interval, Graph, Dataset):-
  rdf_create_next_resource(dataset, 'dh-stats', ['Dataset'], Dataset),
  rdf_assert_instance(Dataset, qb:'DataSet', Graph),

  % dct:subject
  rdf_assert(Dataset, dct:subject, Agent, Graph),

  % qb:structure
  assert_datastructure_definition(
    ['sdmx-dimension':timePeriod],
    Property,
    [],
    Graph,
    DataStructureDefinition
  ),
  rdf_assert(Dataset, qb:structure, DataStructureDefinition, Graph),

  % dct:created
  rdf_assert_now(Dataset, dct:created, Graph),

  % Make observations.
  % Performs the measurement defined by `Goal` for all current agent threads,
  % at a single point in time.
  intermittent_thread(
    assert_observation(
      Dataset,
      Property,
      dh_agent_property(Agent, Property),
      Graph
    ),
    fail,
    Interval,
    _,
    []
  ).

