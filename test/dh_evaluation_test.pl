:- module(
  dh_evaluation_test,
  [
       ant_evaluation/0 % Start an evaluation of the ant agents.
  ]
).


/** <module> DataHives: evluation_test

Evaluation / comparison of the different kind of agents

@author Baudouin Duthoit
@version 2014/06
*/

:-use_module(dh_test(dh_test)).
:-use_module(dh_core(dh_population)).
:-use_module(library(pce_config)).

% Start an evaluation of the ant agents.

ant_evaluation:-
  forall(between(1,10,_),dh_ant_test),
  evaluation_loop.

evaluation_loop:-
  open('data/data.csv',append,Stream),
  sleep(500),
  number_of_deduced_triples(D),
  get_time(T),
  write(Stream,T),
  write(Stream,';'),
  write(Stream,D),
  nl(Stream),
  format([bg(blue)],'~w','InTheFile'),
  close(Stream),
  evaluation_loop.


