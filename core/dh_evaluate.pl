:- module(
  dh_evaluate,
  [
    default_evaluation/0,
    no_evaluation/0
  ]
).

/** <module> DataHives: evaluation

Evaluation predicates for DataHives agents.

@author Wouter Beek
@version 2014/06
*/

:- use_module(library(random)).



default_evaluation:-
  random(1, 10, Rnd),
  (
    Rnd =:= 10
  ->
    thread_exit(done)
  ;
    true
  ).


no_evaluation.

