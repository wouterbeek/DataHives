:- module(
  dh_stats_read,
  [
    dh_stats_collection/2, % +Alias
                           % -Collection:list(pair(term,list))
    dh_stats_graph/3 % +Dataset:iri
                     % +Agent:iri
                     % -Graph:list
  ]
).

/** <module> DataHives Statistics: read

Predicates for reading off statistic results from various measurements
performed in DataHives.

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(aggregate)).
:- use_module(library(error)).
:- use_module(library(pairs)).

:- use_module(dh_stats(dh_stats_core)).



%! dh_stats_collection(+Alias, -Collection:list(pair(term,list))) is det.

dh_stats_collection(Alias, Collection):-
  aggregate_all(
    set(Agent),
    dh_stat(Alias, Agent, _, _),
    Agents
  ),
  findall(
    Agent-Graph,
    (
      member(Agent, Agents),
      dh_stats_graph(Alias, Agent, Graph)
    ),
    Collection
  ).


%! dh_stats_graph(+Dataset:iri, +Agent:iri, -Values:list(pair)) is det.

dh_stats_graph(Dataset, Agent, Values):-
  rdf(Dataset, qb:slice, Slice, dhm),
  rdf(Slice, dho:refAgent, Agent, dhm),
  aggregate_all(
    set(X-Y),
    rdf_datatype(Observation, Property, Value, Datatype, Graph)
    Pairs
  ),
  pairs_values(Pairs, Values).

