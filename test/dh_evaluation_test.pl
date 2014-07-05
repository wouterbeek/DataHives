:- module(
  dh_evaluation_test,
  [
    evaluation/2
  ]
).


/** <module> DataHives: evluation_test

Evaluation / comparison of the different kind of agents

@author Baudouin Duthoit
@version 2014/06
*/


:- use_module(dh(rdf_random_dbpedia)).
:- use_module(dh_agent(dh_agent_ant)). % Agent definition.
:- use_module(dh_agent(dh_agent_bee)). % Agent definitions.
:- use_module(dh_agent(dh_agent_random)). % Agent definition.
:- use_module(dh_core(dh_agent)).

:-use_module(dh_core(dh_population)).

% Start an evaluation of the agents.

evaluation(Type,N):-
  gtrace,
  rdf_random_dbpedia_triple(X),
  create_agents(N, Type,X),
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


