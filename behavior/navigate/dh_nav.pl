:- module(
  dh_nav,
  [
    increment_number_of_steps/0,
    increment_number_of_steps/1 % +Increment:integer
  ]
).

/** <module> DataHives navigate

Reexports the navigation stategies in DataHives.

@author Wouter Beek
@version 2104/07-2014/09
*/

:- use_module(plRdf(rdfs_build2)).

:- reexport(dh_nav(dh_random_jump)).
:- reexport(dh_nav(dh_random_walk)).
:- reexport(dh_nav(dh_weighted_walk)).

:- use_module(dh_agent(dh_agent)).
:- use_module(dh_core(dh_messages)).

%! number_of_steps(-NumberOfSteps:nonneg) is det.

:- thread_local(number_of_steps/1).

:- dynamic(dh:dh_agent_property/2).
:- multifile(dh:dh_agent_property/2).
:- dynamic(dh:dh_agent_property/3).
:- multifile(dh:dh_agent_property/3).

:- initialization(init_agent_properties).



%! increment_number_of_steps is det.

increment_number_of_steps:-
  increment_number_of_steps(1).

%! increment_number_of_steps(+Increment:integer) is det.

increment_number_of_steps(N2):-
  retract(number_of_steps(N1)), !,
  N3 is N1 + N2,
  assert(number_of_steps(N3)).
increment_number_of_steps(N):-
  assert(number_of_steps(N)).



% AGENT PROPERTY

dh:dh_agent_property(Property, Steps):-
  rdf_global_id(dho:steps, Property),
  (   number_of_steps(Steps)
  ->  true
  ;   Steps = 0
  ).

dh:dh_agent_property(Agent, Property, Steps):-
  rdf_global_id(dho:steps, Property),
  dh_agent(Agent),
  dh_agent_ask(Agent, dh:dh_agent_property(Property), Steps).

init_agent_properties:-
  rdfs_assert_property(
    dho:steps,
    dho:agentProperty,
    dho:'Agent',
    xsd:nonNegativeInteger,
    steps,
    'The number of steps an agent has navigated over RDF graphs \c
     since its creation.',
    dh
  ).

