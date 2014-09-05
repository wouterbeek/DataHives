:- module(
  dh_stats_read,
  [
    dh_stats_collection/2, % +Alias
                           % -Collection:list(pair(term,list))
    dh_stats_graph/3 % +Alias
                     % +Agent
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


%! dh_stats_graph(+Alias, +Agent, -Values:list) is det.

dh_stats_graph(Alias, _, _):-
  \+ dh_stat(Alias, _, _, _), !,
  existence_error(dh_stats_alias, Alias).
dh_stats_graph(Alias, Agent, Values):-
  aggregate_all(
    set(Time-Value),
    dh_stat(Alias, Time, Agent, Value),
    Pairs
  ),
  pairs_values(Pairs, Values).

