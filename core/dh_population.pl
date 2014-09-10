:- module(
  dh_population,
  [
    dh_population_delete/0,
    dh_population_property_name/1, % ?Name:atom
    dh_population_property/2 % ?Name:atom
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
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(thread_ext)).
:- use_module(generics(vox_populi)).

:- use_module(dh_agent(dh_agent)).
:- use_module(dh_agent(dh_agent_create)).
:- use_module(dh_agent(dh_agent_property)).
:- use_module(dh_com(dh_edge_weight)).



%! dh_population_delete is det.

dh_population_delete:-
  dh_population_property(members, Agents),
  maplist(dh_agent_delete, Agents).


%! dh_population_property(+Name:atom, +Value) is semidet.
%! dh_population_property(+Name:atom, -Value) is det.
%! dh_population_property(-Name:atom, -Value) is nondet.

dh_population_property(members, Value):-
  aggregate_all(
    set(Agent),
    dh_agent(Agent),
    Value
  ).
dh_population_property(size, Value):-
  dh_population_property(members, Agents),
  length(Agents, Value).
dh_population_property(Name, Value):-
  dh:dh_agent_property(Name, numeric), !,
  dh_population_property(members, Agents),
  aggregate_all(
    sum(Value0),
    (
      member(Agent, Agents),
      dh:dh_agent_property(Agent, Name, Value0)
    ),
    Value
  ).
dh_population_property(Name, Value):-
  dh_agent_property_name(Name), !,
  dh_population_property(members, Agents),
  aggregate_all(
    set(Value0),
    (
      member(Agent, Agents),
      dh:dh_agent_property(Agent, Name, Value0)
    ),
    Value
  ).


%! dh_population_property_name(+Name:atom) is semidet.
%! dh_population_property_name(-Name:atom) is multi.

dh_population_property_name(members).
dh_population_property_name(size).
dh_population_property_name(Name):-
  dh_agent_property_name(Name, numeric).

