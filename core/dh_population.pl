:- module(
  dh_population,
  [
    total_number_of_cycles/1, % -Cycles:nonneg
    total_number_of_steps/1 % -Steps:nonneg
  ]
).

/** <module> DataHives: population

Population statistics for DataHives

@author Wouter Beek
@version 2014/06
*/

:- use_module(library(lists)).

:- use_module(generics(vox_populi)).



%! total_number_of_cycles(-Cycles:nonneg) is det.

total_number_of_cycles(Cycles):-
  ask(agent_, number_of_cycles, Cycless),
  sum_list(Cycless, Cycles).


%! total_number_of_steps(-Steps:nonneg) is det.

total_number_of_steps(Steps):-
  ask(agent_, number_of_steps, Stepss),
  sum_list(Stepss, Steps).

