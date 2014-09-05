:- module(
  dh_stats,
  [
    dh_effectiveness/1
  ]
).

/** <module> DataHives: statistics

Creates datastructures that contain statistical data that can be easily
fed to an R wrapper.

@author Wouter Beek
@version 2014/09
*/

:- use_module(generics(thread_ext)).

:- use_module(dh_agent(dh_agent_property)).

%! dh_stat(?Goal, ?Time:float, ?Agent:atom, ?Value) is nondet.
% The `Goal` is used to name a specific collection of measurements over time.

:- dynamic(dh_stat/4).

:- meta_predicate(dh_stats_loop(2)).
:- meta_predicate(dh_stats_step(2)).



dh_edffectiveness:-
  dh_stats_loop(dh_effectiveness).


dh_effectiveness(Agent, Effectiveness):-
  dh_agent_age(Agent, Age),
  dh_agent_steps(Agent, Steps),
  Effectiveness is Steps / Age.



% Helpers.

%! dh_stats_loop(:Goal, +Interval:positive_integer) is det.
% Runs an intermittent loop of performing measurements every N seconds.

dh_stats_loop(Goal, N):-
  intermittent_thread(
    dh_stats_step(Goal),
    true,
    N,
    _,
    []
  ).


%! dh_stats_step(:Goal) is det.
% Performs the measurement defined by `Goal` for all current agent threads,
% at a single point in time.

dh_stats_step(Goal):-
  get_time(Time)
  forall(
    dh_agent_thread(Agent),
    (
      call(Goal, Agent, Value),
      assert(dh_stat(Time,Agent,Value))
    )
  ).
