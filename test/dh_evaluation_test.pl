:- module(
  dh_evaluation_test,
  [
       ant_evaluation/0
  ]
).

:-use_module(library(semweb/rdf_db)).

:-use_module(dh_test(dh_test)).

ant_evaluation:-
  forall(between(1,10,_),dh_ant_test),
  setup_call_cleanup(
    open('data/data.csv',write,Stream),
    evaluation_loop(Stream),
    close(Stream)
  ).

evaluation_loop(Stream):-
  sleep(500),
  number_of_deduced_triples(D),
  get_time(T),
  write(Stream,T),
  write(Stream,';'),
  write(Stream,D),
  nl(Stream),
  flush,
  format([bg(blue)],'~w','InTHeaFile'),
  evaluation_loop(Stream).

number_of_deduced_triples(T):-
  findall(
    T,
    (
      rdf_graph(G),
      atom_concat('agent_',_,G),
      rdf_statistics(triples_by_graph(G,T))
    ),
    Ts
  ),
  sum_list(Ts,T).



