:- module(
  dh_stats_run,
  [
    y_per_t/3 % :Y
              % +T:positive_integer
              % +Agents:list
  ]
).

/** <module> DataHives Statistics: run

Runs an intermittent loop for performing measurements.

@author Wouter Beek
@version 2014/09
*/

:- use_module(dh_stats(dh_stats_core)).

:- meta_predicate(y_per_t(2,+,+)).



%! y_per_t(:Y, +T:positive_integer, +Agents:list) is det.

y_per_t(Y, T, Agents):-
  dh_stats_loop(y_per_t(Y), Y, T, Agents).



% Tmp

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lambda)).
:- use_module(library(real)).

:- use_module(dh_agent(dh_agent_property)).
:- use_module(dh_core(dh_population)).
:- use_module(dh_stats(dh_stats_read)).



test1:-
  dh_population_property(dho:members, Agents),
  y_per_t(Agents, dho:steps, 100).

test2:-
  dh_stats_collection(
    y_per_t(dho:steps),
    Collection
  ),
  maplist(add_graph, Collection),
  pairs_keys(Collection, [Agent|Agents]),
  <- plot(Agent),
  maplist(<- lines, Agents).

add_graph(Agent-Values):-
  Agent <- Values,
  <- Agent.

