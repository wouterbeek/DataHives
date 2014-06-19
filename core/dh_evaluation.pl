:- module(
  dh_evaluation,
  [
    default_evaluation/0,
    fitness_evaluation/0,
    no_evaluation/0
  ]
).

/** <module> DataHives Evaluation

Evaluation predicates for DataHives agents.

@author Wouter Beek
@version 2014/06
*/

:- use_module(dh_core(dh_action)).



default_evaluation:-
  random_between(1, 10, Rnd),
  (Rnd =:= 10 -> thread_exit(done) ; true).


fitness_evaluation:-
  deductions(Deductions),
  lifetime(Lifetime),
  Fitness is Deductions / Lifetime,
  (Fitness < 0.5 -> thread_exit(done) ; true),
  print_message(informational, fitness(Deductions,Lifetime,Fitness)).

no_evaluation.


% Messages

:- multifile(prolog:message//1).

prolog:message(fitness(Deductions,Lifetime,Fitness)) -->
  {
    thread_self(Me),
    thread_property(Me, alias(MyName))
  },
  ['[~w] ~f (~D deductions; ~D steps)'-[MyName,Fitness,Deductions,Lifetime]].

