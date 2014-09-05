:- module(
  dh_stats_core,
  [
    dh_stat/4, % ?Alias
               % ?Time:float
               % ?Agent
               % ?Value
    dh_stats_loop/4 % +Alias
                    % :Goal
                    % +Agents:list
                    % +Interval:positive_integer
  ]
).

/** <module> DataHives Statistics: Core

The core architecture for performing measurements
and reading off their results.

@author Wouter Beek
@version 2014/09
*/

:- use_module(generics(thread_ext)).

:- use_module(dh_agent(dh_agent_property)).

%! dh_stat(?Alias, ?Time:float, ?Agent, ?Value) is nondet.
% The `Goal` is used to name a specific collection of measurements over time.

:- dynamic(dh_stat/4).

:- meta_predicate(dh_stats_loop(+,2,+,+)).
:- meta_predicate(dh_stats_step(+,2,+)).



%! dh_stats_loop(
%!   +Alias,
%!   :Goal,
%!   +Agents:list,
%!   +Interval:positive_integer
%! ) is det.
% Runs an intermittent loop of performing measurements every N seconds.

dh_stats_loop(Alias, _, _, _):-
  once(dh_stat(Alias, _, _, _)), !,
  existence_error(dh_stats_alias, Alias).
dh_stats_loop(Alias, Goal, Agents, Interval):-
  intermittent_thread(
    dh_stats_step(Alias, Goal, Agents),
    true,
    Interval,
    _,
    []
  ).


%! dh_stats_step(+Alias, :Goal, +Agents:list) is det.
% Performs the measurement defined by `Goal` for all current agent threads,
% at a single point in time.

dh_stats_step(Alias, Goal, Agents):-
  get_time(Time),
  forall(
    member(Agent, Agents),
    (
      call(Goal, Agent, Value),
      assert(dh_stat(Alias, Time, Agent, Value))
    )
  ).

