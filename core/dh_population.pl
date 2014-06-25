:- module(
  dh_population,
  [
    number_of_deduced_triples/1, % -TotaNumberOfDeducedTriples:nonneg
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
:- use_module(library(semweb/rdf_db)).
:- use_module(generics(vox_populi)).



%! total_number_of_cycles(-Cycles:nonneg) is det.

total_number_of_cycles(Cycles):-
  ask(agent_, number_of_cycles, Cycless),
  sum_list(Cycless, Cycles).


%! total_number_of_steps(-Steps:nonneg) is det.

total_number_of_steps(Steps):-
  ask(agent_, number_of_steps, Stepss),
  sum_list(Stepss, Steps).


%! number_of_deduced_triples(-TotaNumberOfDeducedTriples:nonneg) is det

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
