:- module(
  dh_nav,
  [
    increment_number_of_steps/0,
    increment_number_of_steps/1, % +Increment:integer
    number_of_steps/1 % -NumberOfSteps:nonneg
  ]
).

/** <module> DataHives navigate

Reexports the navigation stategies in DataHives.

@author Wouter Beek
@version 2104/07
*/

:- reexport(dh_nav(dh_random_jump)).
:- reexport(dh_nav(dh_random_walk)).
:- reexport(dh_nav(dh_weighted_walk)).

:- thread_local(number_of_steps0/1).



%! increment_number_of_steps is det.

increment_number_of_steps:-
  increment_number_of_steps(1).

%! increment_number_of_steps(+Increment:integer) is det.

increment_number_of_steps(N2):-
  retract(number_of_steps0(N1)), !,
  N3 is N1 + N2,
  assert(number_of_steps0(N3)).
increment_number_of_steps(N):-
  assert(number_of_steps0(N)).


%! number_of_steps(-NumberOfSteps:nonneg) is det.

number_of_steps(N):-
  number_of_steps0(N), !.
number_of_steps(0).

