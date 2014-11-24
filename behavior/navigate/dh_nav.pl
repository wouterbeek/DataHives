:- module(
  dh_nav,
  [
    increment_number_of_steps/0,
    increment_number_of_steps/1, % +Increment:integer
    steps/1 % ?Steps:nonneg
  ]
).

/** <module> DataHives navigate

Reexports the navigation stategies in DataHives.

@author Wouter Beek
@version 2104/07-2014/09
*/

:- use_module(plRdf(rdfs_build2)).

:- reexport(dh(beh/nav/dh_random_jump)).
:- reexport(dh(beh/nav/dh_random_walk)).
:- reexport(dh(beh/nav/dh_weighted_walk)).

:- use_module(dh(agent/dh_agent)).
:- use_module(dh(core/dh_messages)).

%! number_of_steps(-NumberOfSteps:nonneg) is det.

:- thread_local(number_of_steps/1).

:- initialization(init).



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

steps(Steps):-
  number_of_steps(Steps), !.
steps(0).

init:-
  rdfs_assert_property(
    dho:steps,
    dho:agentPropertyLocal,
    dho:'Agent',
    xsd:nonNegativeInteger,
    steps,
    'The number of steps an agent has navigated over RDF graphs \c
     since its creation.',
    dho
  ).

