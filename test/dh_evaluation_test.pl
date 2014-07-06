:- module(
  dh_evaluation_test,
  [
    evaluation/2 % +AgentName:atom
                 % +NumberOfAgents:nonneg
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
:- use_module(dh_core(dh_population)).



%! evaluation(+AgentName:atom, +NumberOfAgents:nonneg) is det.
% Starts an evaluation of the agents.

evaluation(Agent, N):-
gtrace,
  sparql_random_triple(dbpedia, Triple),
  create_agents(N, Agent, Triple),
  evaluation_loop.

evaluation_loop:-
  open('data/data.csv', append, Stream),
  sleep(500),
  get_time(T),
  number_of_deduced_triples(D),
  write(Stream, T),
  write(Stream, ';'),
  write(Stream, D),
  nl(Stream),
  format([bg(blue)], '~w', 'InTheFile'),
  close(Stream),
  evaluation_loop.

