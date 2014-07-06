:- module(
  dh_evaluation_test,
  [
    evaluation/2
  ]
).


/** <module> DataHives: evluation_test

Evaluation / comparison of the different kind of agents

@author Baudouin Duthoit
@author Wouter Beek
@version 2014/06-2014/07
*/

:- use_module(plSparql(sparql_random_triple)).

:- use_module(dh_agent(dh_agent)).
:- use_module(dh_agent(dh_agent_ant)). % Agent definition.
:- use_module(dh_agent(dh_agent_bee)). % Agent definitions.
:- use_module(dh_agent(dh_agent_random)). % Agent definition.
:- use_module(dh_core(dh_population)).

% Start an evaluation of the agents.

evaluation(Type,N):-
  gtrace,
  sparql_random_triple(dbpedia, X),
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

