:- module(
  dh_stats_core,
  [
    dh_stats_loop/4 % :Goal
                    % +Agents:list
                    % +Interval:positive_integer
                    % -Dataset:iri
  ]
).

/** <module> DataHives Statistics: Core

The core architecture for performing measurements
and reading off their results.

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(thread_ext)).

:- use_module(plRdf(datacube)).
:- use_module(plRdf(rdf_build)).

:- meta_predicate(dh_stats_loop(+,1,+,+)).
:- meta_predicate(dh_stats_step(+,1,+)).



%! dh_stats_loop(
%!   :Goal,
%!   +Agents:list(iri),
%!   +Interval:positive_integer,
%!   -Dataset:iri
%! ) is det.
% Runs an intermittent loop of performing measurements every N seconds.

dh_stats_loop(Goal, Agents, Interval, Dataset):-
  rdf_create_next_resource(dataset, dhm, Dataset),
  rdf_assert_now(Dataset, dct:created, dhm),
  aggregate_all(
    set(Slice),
    (
      member(Agent, Agents),
      rdf_create_next_resource(slice, dhm, Slice),
      rdf_assert(Slice, dho:refAgent, Agent, dhm),
      rdf_assert(Dataset, qb:slice, Slice, dhm)
    ),
    Slices
  ),
  intermittent_thread(
    dh_stats_step(Alias, Goal, Slices),
    true,
    Interval,
    _,
    []
  ).


%! dh_stats_step(+Alias, :Goal, +Slices:ordset(iri)) is det.
% Performs the measurement defined by `Goal` for all current agent threads,
% at a single point in time.

dh_stats_step(Alias, Goal, Slices):-
  get_time(Time),
  forall(
    member(Slice, Slices),
    (
      assert_observation(Property, Goal, dhm, Observation),
      rdf_assert(Slice, qd:observation, Observation)
    )
  ).

